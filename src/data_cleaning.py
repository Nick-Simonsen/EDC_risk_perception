import pandas as pd
import os
import re

import manual_zipcodes

def read_csv_files(data_dir: str, country_code: str) -> pd.DataFrame:
    sub_dir = next(sub_dir for sub_dir in os.listdir(data_dir) if country_code in sub_dir)
    csv_file = next(f for f in os.listdir(os.path.join(data_dir, sub_dir)) if f.endswith('.csv'))
    return pd.read_csv(os.path.join(data_dir, sub_dir, csv_file))


def satisficing(df: pd.DataFrame, id_str: str) -> pd.DataFrame:
    df_subset = df.filter(like = 'Q10_')
    df_subset[id_str] = df[id_str]

    df_subset = df_subset.replace(to_replace = r'^(\d) -.*', value = r'\1', regex = True) # Substitute values like "1 - Not at all" with just "1"

    for col in df_subset.columns:
        if col != id_str:
            df_subset[col] = pd.to_numeric(df_subset[col], errors = 'coerce')

    df_subset['Mean score'] = df_subset.drop(columns = [id_str]).mean(axis = 1)
    
    remove_rows = df_subset[df_subset['Mean score'] >= 4]

    print('Rows removed due to satisficing:')
    print(len(remove_rows))
    print(remove_rows[[id_str, 'Mean score']])

    df = df[~df[id_str].isin(remove_rows[id_str])]

    return df


def exclude_participants(df: pd.DataFrame, consent_str: str, id_str: str) -> pd.DataFrame:
    first_row = df.iloc[0]
    df = df.iloc[2:]
    print(df['Consent'].unique())
    df = df[df['Consent'].eq(consent_str) & df['Progress'].eq('100')]

    if 'Screener' in df.columns: # This is only relevant for the Danish dataset
        df = df[df['Screener'] != 'Under 18 år gammel']

    df['Q_RecaptchaScore'] = df['Q_RecaptchaScore'].astype(float)
    remove_rows = df[df['Q_RecaptchaScore'] < 0.5]

    if not remove_rows.empty:
        print('Rows removed due to low RecaptchaScore:')
        print(len(remove_rows))
        print(remove_rows[['ResponseId', 'Q_RecaptchaScore']])

    df = df[df['Q_RecaptchaScore'] >= 0.5]
    df = satisficing(df, id_str)
    df = pd.concat([pd.DataFrame([first_row]), df], ignore_index = True) # We re-add the first row because it's necessary for handling the sequential rankings

    return df


def clean_values(df: pd.DataFrame) -> pd.DataFrame:
    return df.map(lambda x: int(str(x).split('-')[0].strip()) if isinstance(x, str) and '-' in x else x)


def handle_likert_scale(df: pd.DataFrame) -> pd.DataFrame:
    df = df.iloc[1:] # Remove first row (description)
    response_id = df[['ResponseId']]
    risk_dimensions = df.filter(regex = '^Q3_')
    overall_edc_risk = df.filter(regex = '^Q6_')
    trust_dimensions = df.filter(regex = '^Q7_')

    risk_dimensions = clean_values(risk_dimensions)
    overall_edc_risk = clean_values(overall_edc_risk)
    trust_dimensions = clean_values(trust_dimensions)

    return pd.concat([response_id, risk_dimensions, overall_edc_risk, trust_dimensions], axis = 1)


def handle_sequential_rankings(df: pd.DataFrame) -> pd.DataFrame:
    rankings_description = df.iloc[0].filter(regex = '^Q4_')
    rankings_values = df.iloc[1:].filter(regex = '^Q4_')

    first_set = rankings_values.loc[:, rankings_values.columns.str.contains(r'R1_')]
    second_set = rankings_values.loc[:, rankings_values.columns.str.contains(r'R2_')]
    third_set = rankings_values.loc[:, rankings_values.columns.str.contains(r'R3_')]

    # Remove only the first dash, preserving later hypens in the label
    def strip_prefix_after_first_dash(s: str) -> str:
        parts = re.split(r'\s*[-–—]\s*', str(s), maxsplit = 1) # This removes first dash only
        return parts[1] if len(parts) > 1 else s

    formatted_headers = rankings_description.apply(strip_prefix_after_first_dash)
    
    first_set.columns = [f'R1_{formatted_headers[col]}' for col in first_set.columns]
    second_set.columns = [f'R2_{formatted_headers[col]}' for col in second_set.columns]
    third_set.columns = [f'R3_{formatted_headers[col]}' for col in third_set.columns]

    combined_rankings = pd.concat(
        [df.iloc[1:][['ResponseId']], first_set, second_set, third_set], axis = 1
    )

    return combined_rankings


def manual_zipcode_to_region(zip_code: str, country_code: str) -> str:
    """ We manually add regions to zip codes, if API lookup previously failed """
    zip_code = str(zip_code).strip() # Make sure zip code is a string and remove leading/trailing whitespace

    country_dicts = {
        "DK": manual_zipcodes.dk_zips,
        "UK": manual_zipcodes.uk_zips,
        "US": manual_zipcodes.us_zips
    }

    return country_dicts.get(country_code, {}).get(str(zip_code), None)


def convert_zipcode_to_region(df: pd.DataFrame, country_code: str) -> pd.DataFrame:
    def get_region_denmark(zip_code: str) -> str:
        """Fetch region for a single zip code in Denmark."""
        from geopy.geocoders import Nominatim

        try:
            geolocator = Nominatim(user_agent = "zip_to_region_mapper", timeout = 10)
            location = geolocator.geocode(f"{zip_code}, Denmark")
            print(f"Lookup for {zip_code}: {location}") # Log the raw response (mostly just for debugging; partially, because it's nice to see things working/progressing)

            if location:
                address_parts = location.address.split(", ")
                region = next((part for part in address_parts if "Region" in part), None) # Find the part of the address that contains the region
                if region:
                    return region
                    
        except Exception as e:
            print(f"Error fetching region for {zip_code}: {e}")
        return None
    
    def get_region_uk(zip_code: str) -> str:
        """Fetch region for a single zip code in the UK using findthatpostcode.uk API."""
        import requests

        zip_code = zip_code.replace(" ", "").upper()
        url = f"https://api.postcodes.io/postcodes/{zip_code}"
        
        try:
            response = requests.get(url)
            response.raise_for_status() # Raise an exception for 4xx/5xx status codes

            data = response.json()

            if "result" in data and data["result"]:
                region = data["result"].get("region", None)
                country = data["result"].get("country", None)

                # Special handling for Scotland and Northern Ireland
                if country == "Scotland":
                    region = "Scotland"
                elif country == "Northern Ireland":
                    region = "Northern Ireland"

                if region and "the Humber" in region: # Some Yorkshire and The Humber postcodes are returned with lowercase "the", so we need to handle that
                    region = "Yorkshire and The Humber"
                
                return region
            
        except Exception as e:
            print(f"Error fetching region for {zip_code}: {e}")
        return None

    def get_region_us(zip_code: str) -> str:
        """Fetch region for a single zip code in the US."""
        from uszipcode import SearchEngine
        # uszipcode requires sqlalchemy-mate==2.0.0.0

        census_regions = {
            "Northeast": ["CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"],
            "Midwest": ["IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI"],
            "South": ["AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"],
            "West": ["AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"]
        }

        search = SearchEngine()
        
        try:
            result = search.by_zipcode(zip_code)
            state = result.state

            for region, states in census_regions.items():
                if state in states:
                    return region
            
        except Exception as e:
            print(f"Error fetching region for {zip_code}: {e}")
        return None
    
    # Map country codes to their respective functions
    country_code_to_function = {
        'DK': get_region_denmark,
        'UK': get_region_uk,
        'US': get_region_us
    }

    # Get the appropriate function based on the country code
    get_region = country_code_to_function.get(country_code)
    if not get_region:
        raise ValueError(f"Unsupported country code: {country_code}")
    
    regions = df['Q9_9Po'].apply(lambda zip_code: get_region(zip_code) if not pd.isna(zip_code) else None)
    regions = regions.replace("", None)
    df['region'] = regions

    # If has zip code but API lookup failed (region is None/empty), manually add region from constructed dictionary (manual_zipcodes.py)
    manual_regions = df[df['region'].isna()]['Q9_9Po'].map(lambda x: manual_zipcode_to_region(x, country_code))
    df.loc[manual_regions.index, 'region'] = manual_regions

    return df


def demographics_data(df: pd.DataFrame, country_code: str) -> pd.DataFrame:
    all_demographics = df.filter(regex = '^Q8_|^Q9_')

    knowledge = all_demographics.filter(regex = '^Q8_')[1:]
    knowledge = clean_values(knowledge)

    demographics = all_demographics.filter(regex = '^Q9_')[1:]

    # If Q9_1Ge != 'Mand', 'Male', 'Kvinde', or 'Female', replace current value with value from Q9_1Ge_3_TEXT in corresponding row
    demographics['Q9_1Ge'] = demographics.apply(
        lambda x: x['Q9_1Ge_3_TEXT'] if x['Q9_1Ge'] not in
        ['Mand', 'Male', 'Kvinde', 'Female'] else x['Q9_1Ge'], axis = 1
    )

    demographics = demographics.drop(columns = ['Q9_1Ge_3_TEXT', 'Q9_2Age', 'Q9_9Po'])
    demographics = demographics.rename(columns = {'Q9_2Age_1_TEXT': 'Q9_2Age', 'Q9_9Po_1_TEXT': 'Q9_9Po'})

    demographics = pd.concat([df.iloc[1:][['ResponseId']], knowledge, demographics], axis = 1)
    demographics = convert_zipcode_to_region(demographics, country_code)

    return demographics


def process_country_data(country_code: str, consent_str: str, id_str: str, data_dir: str = "./Raw Data") -> None:
    data = read_csv_files(country_code = country_code, data_dir = data_dir)
    data = exclude_participants(data, consent_str, id_str)
    likert_data = handle_likert_scale(data)
    rankings_data = handle_sequential_rankings(data)
    demographics = demographics_data(data, country_code)

    if not os.path.exists('./Processed Data'):
        os.makedirs('./Processed Data')
    
    likert_data.to_csv(f'./Processed Data/{country_code}_likert_data.csv', index = False)
    rankings_data.to_csv(f'./Processed Data/{country_code}_rankings_data.csv', index = False)
    demographics.to_csv(f'./Processed Data/{country_code}_demographics.csv', index = False)


if __name__ == "__main__":
    process_country_data('DK', 'Jeg bekræfter.', 'ResponseId', '../Raw Data') # Data dir is specified because raw data is outside of src folder for privacy reasons (i.e., not uploading non-anonymized data to GitHub)
    # process_country_data('UK', 'I consent.', 'PROLIFIC_PID', '../Raw Data')
    process_country_data('UK', 'I consent.', 'ResponseId', '../Raw Data')
    # process_country_data('US', 'I consent.', 'participantId', '../Raw Data')
    process_country_data('US', 'I consent.', 'ResponseId', '../Raw Data')
