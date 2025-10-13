import cdsapi

dataset = "satellite-land-cover"
request = {
    "variable": "all",
    "year": ["2022"],
    "version": ["v2_1_1"],
    "area": [30, -120, -40, 120]
}

client = cdsapi.Client()
client.retrieve(dataset, request).download()

