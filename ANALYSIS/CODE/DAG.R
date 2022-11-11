library(dagitty)
library(rethinking)
pdf('ANALYSIS/RESULTS/DAG.pdf')

dag = dagitty('dag{
              AFR -> Mimicry;
              AFR -> RelBrainSize;
              AFR -> Longevity;
              Sociality -> Mimicry;
              Sociality -> RelBrainSize;
              Sociality -> Longevity;
              RelBrainSize -> Mimicry;
              RelBrainSize -> Longevity;
              BodySize -> Mimicry;
              BodySize -> Longevity;
              BodySize -> AFR;
              Longevity -> Mimicry}') 
drawdag(dag)

dag = dagitty('dag{
              Sociality -> MimicryPresent;
              Sociality -> RelBrainSize;
              Sociality -> Longevity;
              Sociality -> Quality;
              Sociality -> NumMimics;
              Context -> MimicryDetected;
              Context -> NumMimicsObs;
              Context -> Quality;
              Context -> Template;
              Template -> MimicryDetected;
              Template -> NumMimicsObs;
              Template -> Quality;
              Quality -> MimicryDetected;
              RelBrainSize -> MimicryPresent;
              RelBrainSize -> Longevity;
              RelBrainSize -> NumMimics;
              RelBrainSize -> Quality;
              RelBrainSize -> Longevity;
              BodySize -> Quality;
              BodySize -> Longevity;
              BodySize -> Context;
              Longevity -> MimicryPresent;
              Longevity -> NumMimics;
              Longevity -> Quality;
              NumMimics -> MimicryDetected;
              MimicryPresent -> MimicryDetected;
              Time -> MimicryDetected;
              Time -> NumMimicsObs;
              NumMimics -> NumMimicsObs;
              Quality -> NumMimicsObs
              }') 
drawdag(dag)


dev.off()
impliedConditionalIndependencies(dag)
