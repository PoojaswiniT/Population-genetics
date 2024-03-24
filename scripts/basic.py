import pandas as pd
import re

# Read Excel file into a DataFrame
df = pd.read_excel("AADR Annotation.xlsx")

# Open the output file
with open("AADR_test", 'w', encoding="utf-8") as outfile:
    # Write the header to the output file
    outfile.write("Genetic ID" + "\t" + "Date" + "\t" + "Locality" + "\t" + "Country" + "\t" + "Y haplogroup ISOGG" + '\n')

    # Iterate through rows of the DataFrame
    for index, row in df.iterrows():
        genetic_id = row['Genetic ID']
        date = row[
            'Date mean in BP in years before 1950 CE [OxCal mu for a direct radiocarbon date, and average of range for a contextual date]']
        locality = row['Locality']
        political_entry = row['Political Entity']
        Y_haplogroup = row['Y haplogroup (manual curation in ISOGG format)']
        

        # Use regex to check for 'n/a', 'nan', 'na', 'not published in paper', or empty strings in any of the columns
        if not any(
                re.search(r'n/a|^$|^nan$|^na$|^not published in paper$', str(value)) for value in
                [genetic_id, date, locality, political_entry, Y_haplogroup]):
            if not any(re.search(r'\.\.', str(value)) for value in [genetic_id, date, locality, political_entry, Y_haplogroup]):

                date2 = 1950 - date
                if date2 < 0:
                    # Check for hyphen, asterisk, or parentheses in values and exclude them
                    if not any(re.search(r'[-*()]', str(value)) for value in
                               [genetic_id, locality, political_entry, Y_haplogroup]):
                        outfile.write(
                            f"{genetic_id}\t{date2}\t{locality}\t{political_entry}\t{Y_haplogroup}\n")
                else:
                    # Check for hyphen, asterisk, or parentheses in values and exclude them
                    if not any(re.search(r'[-*()]', str(value)) for value in
                               [genetic_id, locality, political_entry, Y_haplogroup]):
                        # Write the data to the output file
                        outfile.write(
                            f"{genetic_id}\t{date2}\t{locality}\t{political_entry}\t{Y_haplogroup}\n")
