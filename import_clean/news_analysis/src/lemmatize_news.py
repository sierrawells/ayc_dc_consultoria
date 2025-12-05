"""
Lemmatize news article titles and descriptions from Google News scraped data using SpaCy.

This script reads a CSV file containing scraped Spanish news articles and lemmatizes
the 'title' and 'description' columns using SpaCy's Spanish language model. 
The lemmatized results are saved to a new CSV file.

Requirements (must run source .venv/bin/activate prior):
    - spacy
    - pandas
    - gitpython
    - pathlib
    - Spanish language model: python -m spacy download es_core_news_sm

Input:
    - CSV file with 'title' and 'description' columns containing Spanish text

Output:
    - CSV file with original data plus 'title_lemmas' and 'description_lemmas' columns
"""

import pandas as pd
import spacy
from pathlib import Path
from git import Repo

# Load Spanish language model
# es_core_news_sm is SpaCy's small Spanish model for efficient lemmatization
nlp = spacy.load("es_core_news_sm")

# Find git repository root
repo = Repo(Path(__file__).parent, search_parent_directories=True)
REPO_ROOT = Path(repo.working_tree_dir)

# Define file paths
INPUT_FILE = REPO_ROOT / "import_clean/news_analysis/input/google_news_scraped.csv"
OUTPUT_FILE = REPO_ROOT / "import_clean/news_analysis/output/google_news_scraped_lemmatized.csv"

def standardize_proper_nouns(lemmas):
    """
    Standardize references to common proper nouns in Mexico.
    
    Args:
        lemmas (list): List of lemmas as strings
    Returns:
        list: List of lemmas with standardized names
    
    Examples (doctests):
        >>> standardize_proper_nouns(['andrés', 'manuel', 'lópez', 'obrador', 'anunciar'])
        ['amlo', 'anunciar']
        
        >>> standardize_proper_nouns(['andrés', 'manuel', 'presentar'])
        ['amlo', 'presentar']
        
        >>> standardize_proper_nouns(['lópez', 'obrador', 'declarar'])
        ['amlo', 'declarar']
        
        >>> standardize_proper_nouns(['claudia', 'sheinbaum', 'pardo', 'proponer'])
        ['sheinbaum', 'proponer']
        
        >>> standardize_proper_nouns(['claudia', 'decir'])
        ['sheinbaum', 'decir']
        
        >>> standardize_proper_nouns(['claudio', 'x', 'gonzález', 'criticar'])
        ['claudio x gonzález', 'criticar']

        >>> standardize_proper_nouns(['claudio', 'x', 'méxico'])
        ['claudio x gonzález', 'méxico']
        
        >>> standardize_proper_nouns([])
        []
    """
    i = 0

    while i < len(lemmas):

        # 4-word replacements
        if i + 3 < len(lemmas):
            # Replace 'andrés manuel lópez obrador' with 'amlo'
            if (lemmas[i] == 'andrés' and lemmas[i + 1] == 'manuel' and 
                lemmas[i + 2] == 'lópez' and lemmas[i + 3] == 'obrador'):
                lemmas[i] = 'amlo'
                del lemmas[i + 1:i + 4]

        # 3-word replacements
        if i + 2 < len(lemmas):
            # Replace 'claudia sheinbaum pardo' with 'sheinbaum'
            if (lemmas[i] == 'claudia' and lemmas[i + 1] == 'sheinbaum' and 
                lemmas[i + 2] == 'pardo'):
                lemmas[i] = 'sheinbaum'
                del lemmas[i + 1:i + 3]
            # Replace 'claudio x gonzález' with 'claudio x gonzález'
            elif (lemmas[i] == 'claudio' and lemmas[i + 1] == 'x' and 
                  lemmas[i + 2] == 'gonzález'):
                lemmas[i] = 'claudio x gonzález'
                del lemmas[i + 1:i + 3]
            # Replace 'ciudad de méxico' with 'cdmx'
            elif (lemmas[i] == 'ciudad' and lemmas[i + 1] == 'de' and 
                  lemmas[i + 2] == 'méxico'):
                lemmas[i] = 'cdmx'
                del lemmas[i + 1:i + 3]
        
        # 2-word replacements
        if i + 1 < len(lemmas):
            # Replace 'andrés manuel' with 'amlo'
            if lemmas[i] == 'andrés' and lemmas[i + 1] == 'manuel':
                lemmas[i] = 'amlo'
                del lemmas[i + 1]
            # Replace 'lópez obrador' with 'amlo'
            elif lemmas[i] == 'lópez' and lemmas[i + 1] == 'obrador':
                lemmas[i] = 'amlo'
                del lemmas[i + 1]
            # Replace 'claudia sheinbaum' with 'sheinbaum'
            elif lemmas[i] == 'claudia' and lemmas[i + 1] == 'sheinbaum':
                # Replace "claudia sheinbaum" with 'sheinbaum'
                lemmas[i] = 'sheinbaum'
                del lemmas[i + 1]
            # Replace 'sheinbaum pardo' with 'sheinbaum'
            elif lemmas[i] == 'sheinbaum' and lemmas[i + 1] == 'pardo':
                lemmas[i] = 'sheinbaum'
                del lemmas[i + 1]
            # Replace 'claudio x' with 'claudio x gonzález'
            elif lemmas[i] == 'claudio' and lemmas[i + 1] == 'x':
                lemmas[i] = 'claudio x gonzález'
                del lemmas[i + 1]

        # Replace 'claudia' alone with 'sheinbaum'
        if lemmas[i] == 'claudia':
            lemmas[i] = 'sheinbaum'

        i += 1

    return lemmas

def lemmatize_text(text):
    """
    Lemmatize Spanish text using SpaCy and return cleaned lemmas in lowercase.
    
    Args:
        text (str): Input text to lemmatize
        
    Returns:
        list: List of lowercase lemmas as strings (excluding punctuation and stop words)
        
    Note:
        Returns empty list if text is NaN or empty
        Lemmas are the base/dictionary forms of words (e.g., "corriendo" -> "correr")
        Stop words are function words like articles (el, la), prepositions (de, en), etc.
        All lemmas are converted to lowercase and cleaned of non-alphabetical characters
        Lemmas with no alphabetical characters after cleaning are excluded
    """
    # Handle missing or empty values
    if pd.isna(text) or not text:
        return []
    
    # Process text through SpaCy pipeline
    doc = nlp(text)
    
    # Extract and clean lemmas
    lemmas = []
    for token in doc:
        # Skip punctuation and stop words
        if token.is_punct or token.is_stop:
            continue
        
        # Convert to lowercase and remove non-alphabetical characters
        cleaned_lemma = ''.join(char for char in token.lemma_.lower() if char.isalpha())
        
        # Only add if lemma contains alphabetical characters after cleaning
        if cleaned_lemma:
            lemmas.append(cleaned_lemma)
    
    # Standardize references to key political figures
    lemmas = standardize_proper_nouns(lemmas)
    
    return lemmas


def main():
    """
    Main execution function.
    
    Workflow:
        1. Load CSV file
        2. Lemmatize titles and descriptions
        3. Save results to new CSV
    """
    # Read CSV file into pandas DataFrame
    print(f"Reading data from {INPUT_FILE}...")
    df = pd.read_csv(INPUT_FILE)
    
    print(f"Found {len(df)} articles")
    
    # Apply lemmatization to each title in the 'title' column
    # Returns a pandas Series with lists of lemmas
    print("Lemmatizing titles...")
    df['title_lemmas'] = df['title'].apply(lemmatize_text)
    
    # Apply lemmatization to each description in the 'description' column
    print("Lemmatizing descriptions...")
    df['description_lemmas'] = df['description'].apply(lemmatize_text)
    
    # Convert all column names to lowercase for consistency
    df.columns = df.columns.str.lower()

    # Rename first column to 'id_nota'
    df.rename(columns={df.columns[0]: 'id_nota'}, inplace=True)
    
    # Save DataFrame with lemmatized columns to output file
    print(f"Saving results to {OUTPUT_FILE}...")
    df.to_csv(OUTPUT_FILE, index=False)
    
    print("Lemmatization complete!")
    
    # Display sample of results for verification
    print("\nSample of lemmatized titles and descriptions:")
    for idx in [0, 1, 194]:
        print(f"\n--- Article {idx + 1} ---")
        print(f"Title: {df.iloc[idx]['title']}")
        print(f"Title lemmas: {df.iloc[idx]['title_lemmas']}")
        print(f"Description: {df.iloc[idx]['description']}")
        print(f"Description lemmas: {df.iloc[idx]['description_lemmas']}")


if __name__ == "__main__":
    main()
