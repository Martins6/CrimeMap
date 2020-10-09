# CrimeMap

An app to visualise, map and predict crime statistics. It serves as a Proof of Concept for future web applications for major cities governments and general public interest. It uses data from the City of São Paulo.

## Usage or installation

I've built a docker image containing the app. If you don't have Docker installed please refer to this [site](https://www.docker.com/) to more information. If you do have Docker installed, just use the following commands in your terminal:

```
docker pull adrielmartins/crimemap_sp
docker run -it -p 3838:3838 adrielmartins/crimemap_sp
```

Then just acess your local host on port 3838 by typing in your browser `127.0.0.1:3838`, and voila! There should be an app there.

But, if you wish to run in your own computer, first clone this repository. Then, you must have R software installed. Run the R script "setup_docker.R" to install all the dependencies. Set the working directory in R as the "app" directory. Finally, just run the file "app.R".

## Contributing and Future Improvements

Check the project area [here](https://github.com/Martins6/CrimeMap/projects/1), in order to see the plans for the future. This app should focus on risk-related metrics or modelling for crime-analysis. I would be glad to receive any suggestions.

Pull requests are specifically very much welcome! For major changes, please open an issue first to discuss what you would like to change. Otherwise, feel free to tweak and improve it however you want!

## Acknowledgments

This app is from the R community to the world. All the packages that made the app were free and open-source, that is amazing and I'm very thankful. Also, I would like to highlight the [book](https://maczokni.github.io/crimemapping_textbook_bookdown/) "Crime Mapping in R" made by Medina and Solymosi. That book was a real help.

## License
[GPL - 3.0](https://choosealicense.com/licenses/gpl-3.0/)
