![Alt text](chroneon.png)

# Chroneon

A web app dashboard to record and train models to predict gestures from EMG signals.

# Purpose

This shiny web application is intended to provide a user friendly experience for researchers
using EMG (or any other time series signals) for classification purposes.

# Installation

This app is a stand-alone application which can be run by simply the command:  `R ./chroneon.R`. Currently, the application queries `influxdb`- a fast time-series database for the most recent entries. For my use-case, I have a bluetooth device recording movement (EMG signals) and a `C++` application writing data to `influxdb`.

# Recording

Below is a snapshot of the user-defined settings possible during the recording phase. The streaming signals shown in the graph are color coded denoting the different signals. 

### The steps in the following demo are:

> 1. Set desired window frame
> 2. Set User
> 3. Set Gesture
> 4. Click to save recording frame
> 5. Scroll to view signal features from frame

### Demo 

![Alt text](http://g.recordit.co/SfSKyG7TEB.gif)

# Predicting

Below is a snapshot of the user-defined settings possible during the prediction phase. As before the streaming signals are color coded denoting different signals.

### The steps in the following demo are:

> 1. Set desired window frame
> 2. Select user-training set.
> 3. Select desired model/classifier.
> 4. Prep gesture needing prediction with label.
> 5. Click to produce predictions on recording frame.
> 6. Scroll to view results.

### Demo
![Alt text](http://g.recordit.co/Jxgz0ifOxB.gif)

# Conclusions

The above web app gives researchers the ability to stream-record-process multiple signals simultaneously. 

If there are any questions send me a message.
