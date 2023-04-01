
conda activate /home/zyj/miniconda2/envs/qiime2-2019.7

mkdir output

qiime tools import \
  --type 'SampleData[PairedEndSequencesWithQuality]' \
  --input-path manifest.txt  \
  --input-format PairedEndFastqManifestPhred33 \
  --output-path output/paired-end-demux.qza

qiime demux summarize \
  --i-data output/paired-end-demux.qza \
  --o-visualization output/paired-end-demux.qzv 
 
time qiime cutadapt trim-paired \
  --i-demultiplexed-sequences output/paired-end-demux.qza \
  --p-cores 10 \
  --p-front-f AACMGGATTAGATACCCKG \
  --p-front-r ACGTCATCCCCACCTTCC \
  --o-trimmed-sequences output/trimmed-seqs.qza  \
  --verbose

qiime vsearch join-pairs \
  --i-demultiplexed-seqs output/trimmed-seqs.qza \
  --o-joined-sequences output/joined-demux.qza

qiime quality-filter q-score-joined \
  --i-demux output/joined-demux.qza \
  --p-min-quality 25 \
  --o-filtered-sequences output/demux-joined-filtered.qza \
  --o-filter-stats output/demux-joined-filter-stats.qza 

qiime demux summarize \
  --i-data output/demux-joined-filtered.qza \
  --o-visualization output/demux-joined-filtered.qzv
  
qiime deblur denoise-16S \
  --i-demultiplexed-seqs output/demux-joined-filtered.qza \
  --p-left-trim-len 0 \
  --p-trim-length 350 \
  --p-jobs-to-start 10 \
  --p-sample-stats \
  --o-representative-sequences output/deblur-rep-seqs.qza \
  --o-table output/deblur-table.qza \
  --o-stats output/deblur-stats.qza 

qiime feature-table summarize \
  --i-table output/deblur-table.qza \
  --o-visualization output/deblur-table.qzv \
  --m-sample-metadata-file metadata.txt
  
qiime feature-classifier classify-sklearn \
  --i-classifier ../../../training-feature-classifiers/silva_132_97_16S_V57classifier.qza \
  --i-reads output/deblur-rep-seqs.qza \
  --o-classification output/taxonomy-silva.qza
  
qiime taxa barplot \
  --i-table output/deblur-table.qza \
  --i-taxonomy output/taxonomy-silva.qza \
  --m-metadata-file metadata.txt \
  --o-visualization output/taxa-bar-plots.qzv

qiime feature-table relative-frequency \
  --i-table output/deblur-table.qza \
  --o-relative-frequency-table output/relative-frequency-table.qza

qiime tools export \
  --input-path output/deblur-table.qza \
  --output-path output/deblur-table.biom \
  --output-format BIOMV210Format 

qiime tools export \
  --input-path output/relative-frequency-table.qza \
  --output-path output/relative-frequency-table.biom \
  --output-format BIOMV210Format 

qiime tools export \
  --input-path output/taxonomy-silva.qza \
  --output-path output/taxonomy.tsv \
  --output-format TSVTaxonomyFormat 

biom add-metadata \
  -i output/deblur-table.biom \
  -o output/otu_table_tax.biom \
  --observation-metadata-fp output/taxonomy.tsv \
  --sc-separated taxonomy \
  --observation-header OTUID,taxonomy 

biom add-metadata \
  -i output/relative-frequency-table.biom  \
  -o output/otu_table_tax_relative.biom \
  --observation-metadata-fp output/taxonomy.tsv \
  --sc-separated taxonomy \
  --observation-header OTUID,taxonomy

biom convert \
  -i output/otu_table_tax.biom \
  -o output/otu_table_tax.tsv \
  --header-key taxonomy \
  --to-tsv 

biom convert \
  -i output/otu_table_tax_relative.biom \
  -o output/otu_table_tax_relative.tsv \
  --header-key taxonomy \
  --to-tsv 

qiime feature-table tabulate-seqs \
  --i-data output/deblur-rep-seqs.qza \
  --o-visualization output/deblur-rep-seqs.qzv

qiime alignment mafft \
  --i-sequences output/deblur-rep-seqs.qza \
  --o-alignment output/aligned-rep-seqs.qza

qiime alignment mask \
  --i-alignment output/aligned-rep-seqs.qza \
  --o-masked-alignment output/masked-aligned-rep-seqs.qza

qiime phylogeny fasttree \
  --i-alignment output/masked-aligned-rep-seqs.qza \
  --o-tree output/unrooted-tree.qza

qiime phylogeny midpoint-root \
  --i-tree output/unrooted-tree.qza \
  --o-rooted-tree output/rooted-tree.qza

qiime diversity core-metrics-phylogenetic \
  --i-table output/deblur-table.qza \
  --i-phylogeny output/rooted-tree.qza \
  --p-sampling-depth 5057 \
  --m-metadata-file metadata.txt \
  --output-dir output/core-metrics-results
# the parameter for --p-sampling-depth, usually use the lowest feature count of sample 

qiime diversity alpha-group-significance \
  --i-alpha-diversity output/core-metrics-results/faith_pd_vector.qza \
  --m-metadata-file metadata.txt \
  --o-visualization output/core-metrics-results/faith-pd-group-significance.qzv

qiime diversity alpha-group-significance \
  --i-alpha-diversity output/core-metrics-results/evenness_vector.qza \
  --m-metadata-file metadata.txt \
  --o-visualization output/core-metrics-results/evenness-group-significance.qzv

qiime diversity alpha-group-significance \
  --i-alpha-diversity output/core-metrics-results/observed_otus_vector.qza \
  --m-metadata-file metadata.txt \
  --o-visualization output/core-metrics-results/observed_otus-group-significance.qzv

qiime diversity alpha-group-significance \
  --i-alpha-diversity output/core-metrics-results/shannon_vector.qza \
  --m-metadata-file metadata.txt \
  --o-visualization output/core-metrics-results/shannon-group-significance.qzv

qiime tools extract \
  --input-path output/core-metrics-results/evenness-group-significance.qzv \
  --output-path output/core-metrics-results/evenness-group-significance/
  
qiime tools extract \
  --input-path output/core-metrics-results/faith-pd-group-significance.qzv \
  --output-path output/core-metrics-results/faith-pd-group-significance/  
   
qiime tools extract \
  --input-path output/core-metrics-results/observed_otus-group-significance.qzv \
  --output-path output/core-metrics-results/observed_otus-group-significance/  
  
qiime tools extract \
  --input-path output/core-metrics-results/shannon-group-significance.qzv \
  --output-path output/core-metrics-results/shannon-group-significance/

qiime tools export \
  --input-path output/core-metrics-results/bray_curtis_distance_matrix.qza \
  --output-path output/core-metrics-results/bray_curtis_distance_matrix
  
qiime tools export \
  --input-path output/core-metrics-results/jaccard_distance_matrix.qza \
  --output-path output/core-metrics-results/jaccard_distance_matrix
   
qiime tools export \
  --input-path output/core-metrics-results/unweighted_unifrac_distance_matrix.qza \
  --output-path output/core-metrics-results/unweighted_unifrac_distance_matrix
     
qiime tools export \
  --input-path output/core-metrics-results/weighted_unifrac_distance_matrix.qza \
  --output-path output/core-metrics-results/weighted_unifrac_distance_matrix

qiime tools export \
  --input-path output/core-metrics-results/rarefied_table.qza \
  --output-path output/core-metrics-results/rarefied_table
  
qiime tools export \
  --input-path output/core-metrics-results/shannon_vector.qza \
  --output-path output/core-metrics-results/shannon_vector.tsv \
  --output-format AlphaDiversityFormat 
    
qiime tools export \
  --input-path output/core-metrics-results/observed_otus_vector.qza \
  --output-path output/core-metrics-results/observed_otus_vector.tsv \
  --output-format AlphaDiversityFormat 
    
qiime tools export \
  --input-path output/core-metrics-results/faith_pd_vector.qza \
  --output-path output/core-metrics-results/faith_pd_vector.tsv \
  --output-format AlphaDiversityFormat 

biom add-metadata \
  -i output/core-metrics-results/rarefied_table/feature-table.biom \
  -o output/core-metrics-results/rarefied_table/rarefied_feature-table.biom \
  --observation-metadata-fp output/taxonomy.tsv \
  --sc-separated taxonomy \
  --observation-header OTUID,taxonomy

biom convert \
  -i output/core-metrics-results/rarefied_table/rarefied_feature-table.biom \
  -o output/core-metrics-results/rarefied_table/rarefied_feature_table.tsv \
  --header-key taxonomy \
  --to-tsv 