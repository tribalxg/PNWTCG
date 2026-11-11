# PNWTCG
Welcome to the Pacific Northwest Tribal Coding Group Github repo!

This repo contains examples, templates, and meeting materials.

The repo and webpage are publicly visible, but to interact with it (eg open or comment on issues, or suggest changes via pull request), you need a GitHub account. It may require you to 'fork' the repo so a copy of it is attached to your account (and this may happen automatically when you click 'edit' links). 

If that's not allowed by your organization, you should still be able to download the repository and edit it locally, and if you get the edits to Ryan by any means, he should be able to integrate them. For instance, you should be able to use RStudio or Positron to download the repository by simply copying its URL (https://github.com/rmcminds/PNWTCG.git), and then change the code and send scripts or the whole repo as a zip file via email.

A [simple app](visNetwork/simple_editable_saveable/app.R) for creating network diagrams.

To render the website locally, open this repository as the working directory in R and run `source("render_website.R")`. This generates `_quarto.yml` and renders the site to `scratch/site/`. The output is ignored by Git and can be copied to a web server.
