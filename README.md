# Code for "Benchmarking of variant pathogenicity prediction methods using a population genetics approach" (2024)

Tested with Hail version 0.2.107 and Snakemake 7.32

## The "Data" pipeline

`data/` contains [Hail](https://hail.is/) and Snakemake code that requires execution in Google Cloud and saves files to a Google Storage bucket. Copies of the generated files are available in `files/`.

### How to run

1. Create a new cluster: `hailctl dataproc start <cluster_name> --packages snakemake --requester-pays-allow-buckets gnomad-public-requester-pays --project <project_name> --bucket <bucket_name> --region <region> --num-workers <N> --image-version=2.0.27-debian10`
2. Connect to the cluster: `gcloud beta compute ssh <user_name>@<cluster_name>-m --zone "<zone>" --project "<project_name>"`
3. `git clone` this repository and navigate to `data/`
4. Run the pipeline: `snakemake --cores all --configfile config.yaml --config gcp_rootdir="<bucket_name>/some_directory/" gcp_username="<user_name>"`

## The "Analysis" pipeline

`analysis/` contains scripts that reproduce the figures from the main paper and the supplement using files created in `data/`.

### How to run

1. `git clone` the code for CAPS (https://github.com/VCCRI/CAPS) into the same root directory as `PopGenVariantFiltering/`
2. Navigate to `PopGenVariantFiltering/analysis/`
3. `snakemake --cores all --config gcp="False"` (faster: uses copies from `files/`) or `snakemake --cores all --config gcp="True" gcp_rootdir="<bucket_name>/some_directory/"` (slower: uses GS files); some files need to be uncompressed first

### Large files

The file `missense_dbnsfp_parsed.tsv.bgz` is tracked with Git LFS and is not included when cloning/downloading the contents of this repository. Please download this file manually.
