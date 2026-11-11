from uszipcode import SearchEngine
# Requires sqlalchemy-mate==2.0.0.0

census_regions = {
    "Northeast": ["CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"],
    "Midwest": ["IL", "IN", "IA", "KS", "MI", "MN", "MO", "NE", "ND", "OH", "SD", "WI"],
    "South": ["AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV"],
    "West": ["AK", "AZ", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY"]
}

search = SearchEngine()

def zip_to_region(zip_code):
    result = search.by_zipcode(zip_code)
    state = result.state
    print(state)
    for region, states in census_regions.items():
        if state in states:
            return region
    return "Unknown"

# Example usage
zip_code = "83427"
print(zip_to_region(zip_code)) # Output: Northeast
