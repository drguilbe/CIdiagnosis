# CIdiagnosis

Centola, Becker, Zhang, Aysola, Guilbeault, and Khoong. 2023. Experimental Evidence for Structured Information-Sharing Networks Reducing Medical Errors.
<br>
<br>
<b>This repository contains</b>: <br>
- the full experimental dataset collected for this study<br>
- an R script that replicates all of our results from the main and supplementary using the experimental data <br>
- a dataset of physician demographics <br>

<b>Structure of the experimental dataset</b>: <br>
<b>trial</b>: refers to the unique trial ID of each group in either the network or control condition<br>
<b>subject_id</b>: unique ID refering to each clinician participant (this is constant across trials) <br>
<b>net</b>: this is the experimental condition (network or control) <br>
<b>vignette</b>: this refers to the specific clinical vignette evaluated by the group<br>
<b>response 1(2,3)</b>: the clinician's diagnostic assessment at round 1(2,3) <br>
<b>discrete_response_1(2,3)</b>: the clinician's treatment recommendation at round 1(2,3)<br>
<b>truth</b>: the correct diagnostic assessment for each vignette<br>
<b>discrete_answer</b>: the highest quality treatment recommendation for each vignette<br>
<b>neighbor_list</b>: each clinicians' network neighbors in a given trial (constant for all rounds of a trial) <br>
<b>neighbor_avg_1(2)</b>: the average diagnostic assessment of each clinician's network peers after round 1(2), shown on round 2(3)<br>
<b>neighbor_estimates_1(2)</b>: the raw estimates of each clinician's network peers on round 1(2) <br>
<br>
Any questions regarding this code or data can be sent to douglas.guilbeault[at]haas.berkeley.edu. 
