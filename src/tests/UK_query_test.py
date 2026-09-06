import requests

def get_region_from_postcode(postcode):
    # Clean up the postcode format (remove spaces and convert to uppercase)
    postcode = postcode.replace(" ", "").upper()
    
    # API endpoint for single postcode lookup
    url = f"https://api.postcodes.io/postcodes/{postcode}"

    try:
        # Make the API request
        response = requests.get(url)
        response.raise_for_status() # Raise an exception for HTTP errors

        data = response.json()

        if "result" in data and data["result"]:
            region = data["result"].get("region", None)
            country = data["result"].get("country", None)

            # Special handling for Scotland and Northern Ireland
            if country == "Scotland":
                region = "Scotland"
            elif country == "Northern Ireland":
                region = "Northern Ireland"

            return region if region else "Region not found"
        else:
            return "Region not found"

    except requests.exceptions.RequestException as e:
        return f"Error fetching data: {e}"

def get_regions_from_postcodes(postcodes):
    results = {}
    for postcode in postcodes:
        results[postcode] = get_region_from_postcode(postcode)
    return results

# Example usage
postcodes = ["NE29 7QG"]
regions = get_regions_from_postcodes(postcodes)
for postcode, region in regions.items():
    print(f"The region for postcode {postcode} is: {region}")