# Script Similarity Analysis App

A instructor-facing Shiny application for analyzing and visualizing code
similarity between R, Rmd, and Qmd scripts. This app helps instructors identify
potential plagiarism, similar approaches, and understand patterns in student
submissions.

## Features

### Similarity Analysis Methods

The app supports three different similarity calculation methods:

1. **Cosine Similarity (Token-based)**: Compares scripts based on token
   frequency vectors using cosine distance
2. **Jaccard Similarity (Set-based)**: Compares unique tokens using Jaccard
   index (intersection/union)
3. **Edit Distance**: Compares raw code using Levenshtein distance, normalized
   by length

### Visualization Methods

Choose from two dimensionality reduction techniques to visualize script similarities:

1. **t-SNE (t-Distributed Stochastic Neighbor Embedding)**:
   - Better for discovering local structure and clusters
   - Adjustable perplexity parameter (1-50)
   - Recommended for larger datasets (>10 scripts)

2. **MDS (Multi-Dimensional Scaling)**:
   - Better for preserving global distances
   - More stable and deterministic
   - Works well with smaller datasets

### Interactive Features

- **2D Scatter Plot**: Interactive visualization where closer points indicate
  more similar scripts
- **Click to View**: Click on any point to view the script content (can open
  multiple files)
- **Similarity Matrix Heatmap**: Hierarchically clustered heatmap showing all
  pairwise similarities
- **Cluster Detection**: Automatic identification of similarity clusters above a
  threshold
- **Top Similar Pairs**: Table showing the most similar script pairs
- **Summary Statistics**: Mean, median, standard deviation of similarities
- **Export Results**: Download all pairwise similarities as CSV

## How to Run the Application:

```r
library(shiny)
runApp(system.file(file.path('shiny', 'similarity_app', 'app.R'), 
                   package='autoharp'))
```

## Usage Instructions

### Basic Workflow

1. **Specify Scripts Folder**:
   - Enter the path to a folder containing R, Rmd, or Qmd files
   - Use the "Browse..." button for a graphical folder selector (if shinyFiles
     is installed)
   - Can use absolute paths (e.g., `/home/user/scripts`) or relative paths
     (e.g., `sol`)
   - The app will automatically find all `.R`, `.Rmd`, and `.qmd` files

2. **Configure Analysis**:
   - **Similarity Method**: Choose how to compare scripts (default: cosine)
   - **Visualization Method**: Choose how to display results (default: t-SNE)
   - **t-SNE Perplexity**: Adjust if using t-SNE (lower for smaller datasets)
   - **Similarity Threshold**: Set threshold for cluster detection (0.5-0.95)

3. **Run Analysis**:
   - Click "Run Similarity Analysis" to process all scripts
   - Progress indicator shows calculation status
   - Results appear in multiple sections below

4. **Explore Results**:
   - **Summary Statistics**: See overall similarity distribution
   - **Scatter Plot**: Explore the 2D projection, click points to view scripts
   - **Heatmap**: See all pairwise similarities in hierarchically clustered heatmap
   - **Similar Pairs**: Review the top 20 most similar script pairs
   - **Clusters**: See detected groups of similar scripts
   - **Full Matrix**: Browse the complete similarity matrix

5. **Export Results**:
   - Click "Download Results" to save all pairwise similarities as CSV
   - File includes Script1, Script2, Similarity score, and Method used

### Understanding the Results

#### Similarity Scores

- **1.0**: Identical scripts (or extremely similar)
- **0.7-0.99**: High similarity - may indicate collaboration or plagiarism
- **0.4-0.69**: Medium similarity - similar approaches or shared patterns
- **0.0-0.39**: Low similarity - different approaches

#### Scatter Plot Interpretation

- **Proximity**: Scripts closer together in the 2D plot are more similar
- **Colors**: Different colors indicate detected clusters (gray = singleton)
- **Interactivity**: Click on points to view script content in a modal
- **Multiple Views**: Can open multiple scripts to compare side-by-side

#### Clusters

- Clusters are groups of scripts with similarity above the threshold
- Useful for identifying potential collaboration or common approaches
- Singleton scripts (not in any cluster) are shown in gray

## Method Selection Guide

### When to Use Each Similarity Method

**Cosine (Token-based)** 
- Default, recommended for most cases:
- Fast and efficient
- Robust to code formatting differences
- Good balance of precision and recall
- Works well for both small and large codebases

**Jaccard (Set-based)** - When order doesn't matter:
- Ignores token frequency (only presence/absence)
- More lenient for varied implementations
- Good for detecting use of similar vocabulary
- Much slower to run on large batch of scripts.

**Edit Distance** - For exact matching:
- Sensitive to whitespace and formatting
- Best for detecting near-exact copies
- More strict than token-based methods
- Slower for large files

### When to Use Each Visualization Method

**t-SNE** - Recommended for most cases:
- Better at revealing local clusters
- More visually intuitive groupings
- Adjust perplexity based on dataset size:
  - Small (5-10 scripts): perplexity = 2-5
  - Medium (10-30 scripts): perplexity = 5-15
  - Large (>30 scripts): perplexity = 15-30

**MDS** - For distance preservation:
- More deterministic (reproducible results)
- Better preserves global structure
- Faster computation
- Works better with small datasets

## Configuration Options

### Similarity Threshold

Controls cluster detection sensitivity:
- **0.5**: Very lenient - large clusters, may include false positives
- **0.7**: Balanced - good for general use (default)
- **0.8**: Strict - only very similar scripts grouped
- **0.95**: Very strict - near-identical scripts only

### t-SNE Perplexity

Balances local vs. global structure:
- **Low (1-5)**: Emphasizes local structure, tighter clusters
- **Medium (5-15)**: Balanced view
- **High (15-30)**: Emphasizes global structure, looser clusters
- **Note**: Automatically adjusted for small datasets

## File Handling

### Supported File Types

- `.R` - R scripts
- `.r` - R scripts (lowercase)
- `.Rmd` - R Markdown files (code chunks extracted)
- `.rmd` - R Markdown files
- `.qmd` - Quarto documents (code chunks extracted)
- `.Qmd` - Quarto documents (code chunks extracted)

### Code Extraction

For Rmd and Qmd files:
- Only code chunks are analyzed (prose is ignored)
- Uses knitr patterns for robust chunk detection
- Handles multi-line chunk options
- Empty chunks are handled gracefully

## Technical Notes

### Performance

- Pairwise similarity: O(n²) where n is number of scripts
- Large datasets (>100 scripts) may take several minutes
- Progress indicator shows calculation status

### Dependencies

Required packages:
- `shiny`: Application framework
- `DT`: Interactive data tables
- `Rtsne`: t-SNE dimensionality reduction
- `stringr`: String manipulation
- `plotly`: Interactive plots
- `shinyFiles`: Folder selection UI
- `knitr`: Required for Rmd/Qmd chunk extraction
