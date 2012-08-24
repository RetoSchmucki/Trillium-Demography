Trillium-Demography
===================

This is a dump for my R-code for my Trillium project

In the orignial code, the contribution of the lower-level vital rate in life table response experiment is computed with the elasticity calculated from the average matrix, the one the cover population in both hedgerows and forests. 

But this assume that the average matrix is the best estimate of population response to change in vital rates.

However, change in life-cycle, including faster growth or lower mortality of adult, can significantly change the elasticity pattern. In other words, subtantial changes in life-cycle transition is expected to affect sensitivity of growth rate to each elements and this would be where we have some potential mechanisms to buffer environmental variation affecting the both the mean vital rates or their variation.

Therefore, contribution of change in vital rate should not be computed using the elasticity calculated on the average matrix, but rather on the elasticity of the specific treatment. 

By comparing the results obtained by the these two approach, we can detangle how change in vital rates associated to environmental change affect not only the growth rate, but also how it affect population sensitivity to these changes. Sensitivity might shift from one vital rate to another as the effect of suboptimal conditions for one life stage might be compensated by the other.
