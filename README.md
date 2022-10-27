# BGVIndexWeb



### Miniconda Install
Go to the official [Miniconda website](https://docs.conda.io/en/latest/miniconda.html) and follow the instructions to install based on your OS. 

### Clone repository

Open the terminal/command console and clone the BGVIndexWeb repo in the desired location in your system
```
git clone https://github.com/ddelgadillod/BGVIndexWeb
```
Alternatively, you can download repository content from [here](https://github.com/ddelgadillod/BGVIndexWeb/archive/refs/heads/main.zip).

Then, open the terminal or Miniconda prompt and activate conda base environment:
```
conda activate
```
In the same terminal, go to the path where the repository was downloaded, then you will see the same files listed at the top of this page.

### Environment setup
Create the conda environment, it contains some dependencies, including a clean version of R 3.6.1:
```
conda env create -f bgvIndexEnv.yaml
```
Activate the bgvIndexEnv environment.

```
conda activate bgvIndexEnv
```

### Installing requirements

from the same path, install the other requirements:
```
Rscript requirements.R
```

### Run & Deploy
to run and easily deploy the App:
```
Rscript runApp.R
```

If all is the right way, you could see something like this on your console:


```
.
.
.
Warning: package ‘data.tree’ was built under R version 3.6.3
Warning: package ‘treemap’ was built under R version 3.6.3

Listening on http://0.0.0.0:3997
[1] 1
[1] "Coffea arabica"
```

If the web browser is not open, use the following URL in your browser: http://localhost:3997/



