# msa is an R package available via the Bioconducter. 

#loading packages
library(ggplot2)
library(ggmsa)
library(msa)
library(Biostrings)

#importing fasta file containing protein sequences
myseq = readAAStringSet("data/mgryph_phyloseq.fasta")
myseq

#performing multiple sequence alignment using clustal omega. Clustalomega is preferred for protein sequences
alignment = msa(myseq, method = "ClustalOmega")
print(alignment)

#save aligned sequences to a FASTA file

#convert alignment to AAStringSet
alignment_set = as(alignment, "AAStringSet")

#save as FASTA
writeXStringSet(alignment_set, file = "mgryph_phyloseq_aligned.fasta")

# visualise with ggmsa package
# importing alignment file
#alignment_file = "mgryph_phyloseq_aligned.fasta"

# Save the aligned sequences with cleaned names
writeXStringSet(alignment_set, file = "mgryph_phyloseq_aligned_named.fasta")

#visualize the alignment
ggmsa(alignment_set, start = 1, end = 50, color = "Chemistry_AA")

#--------------------------------------------------

# creating phylogenetic tree using ggtree package
library(ape)
library(ggtree)
library(phangorn)

# read aligned protein sequences in phangorn compatible format
tree_alignment = read.phyDat("mgryph_phyloseq_aligned.fasta", format = "fasta", type = "AA")

# calculate protein distance matrix
dist_matrix = dist.ml(tree_alignment)

#build the phylogenetic tree using maximum likelihood method
tree = NJ(dist_matrix)

phytree = pml(tree, tree_alignment)
phytree = optim.pml(phytree, model = "JTT")

#check tip labels
print(tree$tip.label)

#root the tree on chosen outgroup
rooted_tree = root(tree, outgroup = "CYP199A4[R.palustris]", resolve.root = TRUE)

# visualize the ggtree
fig = ggtree(rooted_tree) + geom_tiplab()
print(fig)
# for circular layout
cir_fig = ggtree(rooted_tree, layout = "circular") + geom_tiplab()
print(cir_fig)

#saving the tree plot
ggsave("mgryph_phylo_tree.pdf", plot = cir_fig, width = 8, height = 6)


#_______________________________________________________________________________