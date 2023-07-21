# Installation Instructions

**Installing & Running Our Project - Student Center:**

These instructions are given based on the assumption that you have already installed OCAML as given by the CS3110 textbook. If you have not done so, please do so here:

https://cs3110.github.io/textbook/chapters/preface/install.html

Next, prior to running our project, it is required that you first run these two commands in the terminal.

```
$ opam install yojson
$ opam install ANSITerminal
$ brew install openssl
$ opam install cohttp-lwt-unix cohttp-async ssl lwt_ssl
```

Now, it is time to install the .zip file of our project. To do this, go to the link of our github repo:

https://github.coecis.cornell.edu/wph52/final_project_3110

On github, click the green button at the top right that says "Code". From there, click the button at the bottom of that dropdown-window that says "Download ZIP". This will download the .zip file to your device. Then unzip the file into a directory of your choice.

Next, make sure you advance to the proper directory in the terminal, which should be the directory where the .zip was installed, where a folder should exist named "final_project_3110-main".

Your terminal should look like this:

```
 $ ... final_project_3110-main %
```

Finally to run our project, type into the terminal:

```
$ make play
```
A few users you can use to login: 

netid: ocr5
password: 7

netid: asu7
password: 123

If you want to see an example of how we hashed passwords, type in 'iamahackerman' into the netid field, then login with a netid and password from above. 

Enjoy!
