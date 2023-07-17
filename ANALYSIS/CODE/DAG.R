library(dagitty)
library(rethinking)
pdf('ANALYSIS/RESULTS/DAG.pdf')

dag = dagitty('dag{
              AFR -> VPL;
              AFR -> RelBrainSize;
              AFR -> Longevity;
              Sociality -> VPL;
              Sociality -> RelBrainSize;
              Sociality -> Longevity;
              RelBrainSize -> VPL;
              RelBrainSize -> Longevity;
              BodySize -> VPL;
              BodySize -> Longevity;
              BodySize -> AFR;
              Longevity -> VPL}') 
drawdag(dag)

dag = dagitty('dag{
              Sociality -> VPLPresent;
              Sociality -> RelBrainSize;
              Sociality -> Longevity;
              Sociality -> Quality;
              Sociality -> RepertoireSize;
              Context -> VPLDetected;
              Context -> RepertoireSizeObs;
              Context -> Quality;
              Context -> Template;
              Template -> VPLDetected;
              Template -> RepertoireSizeObs;
              Template -> Quality;
              Quality -> VPLDetected;
              RelBrainSize -> VPLPresent;
              RelBrainSize -> Longevity;
              RelBrainSize -> RepertoireSize;
              RelBrainSize -> Quality;
              RelBrainSize -> Longevity;
              BodySize -> Quality;
              BodySize -> Longevity;
              BodySize -> Context;
              Longevity -> VPLPresent;
              Longevity -> RepertoireSize;
              Longevity -> Quality;
              RepertoireSize -> VPLDetected;
              VPLPresent -> VPLDetected;
              Time -> VPLDetected;
              Time -> RepertoireSizeObs;
              RepertoireSize -> RepertoireSizeObs;
              Quality -> RepertoireSizeObs
              }') 
drawdag(dag)


dev.off()
impliedConditionalIndependencies(dag)
