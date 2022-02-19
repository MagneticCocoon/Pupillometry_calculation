# Pupillometry_calculation_2D
Calculation of pupillometry 

## General Description
This is the 2nd study (B2) in IGBL (Indicator of Game-Based Learning) project. 

It is published as:
Lee, J. Y., Donkers, J., Jarodzka, H., Sellenraad, G., & Van Merriënboer, J. J. (2020). Different effects of pausing on cognitive load in a medical simulation game. Computers in Human Behavior, 110, 106385.

## Acknowledgement
This work was supported by the Netherlands Organization for Scientific Research (NWO) [grant numbers 055.16.117]. (https://www.nwo.nl/en/projects/05516117)

## Pupillometry analysis
The merged data set (i.e., eye-tracking data and game logs) was imported into R for data processing. We controlled for three major confounding factors in pupillometry: (1) pupillary light reflex, (2) off- axis distortion, and (3) pupil dilation latency. For the issue of pupil- lary light reflex, all pupil dilation values of a participant were compared against the established individual baseline, calculating the absolute pupil increase in millimeters (Beatty & Lucero-Wagoner, 2000). The pupil dilation values during pauses were compared against a separate baseline for the paused screen. Off-axis distortion occurs when the eye rotates away from the eye camera (Mathur, Gehrmann, & Atchison, 2013). To compensate for this distortion, we used a geometric correction model developed by Hayes and Petrov (2016). For the pupil dilation latency, we removed the pupillometry data during the latency which was set at 800 ms from the onset of different stimuli (i.e., game screen and paused screen) (Ahern & Beatty, 1979).
After this preprocessing, we calculated means of the absolute pupil increase for each trial. To compare different periods (i.e. game-play and pause) in different modes (i.e., intense and plain), means for each period were computed. All pupil diameter samples with a value of 0 were dis- carded. Outliers lower than the first quartile – 1.5 interquartile range (IQR) and higher than the third quartile þ 1.5 IQR were removed (Tukey, 1977).
