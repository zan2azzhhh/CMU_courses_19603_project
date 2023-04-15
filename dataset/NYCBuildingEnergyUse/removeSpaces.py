with open("./nyc_benchmarking_disclosure_data_reported_in_2016.csv") as f:
    s = f.read().splitlines()

    for i in range(100):
        print((s[i].count(",")))
