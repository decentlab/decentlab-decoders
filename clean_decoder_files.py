"""
Utility script to clean up the naming of the decoder files.
This is needed to ensure that the Python decoder files are correctly imported.

For example, within the folder './DL-DWS', the decoder file 
'DL-DWS (f02=232263168,s=0.000302459,m0=1370)' should be renamed to 'DL-DWS.py'.
"""

from pathlib import Path

def clean_decoder_files():
    # get all folders starting with DL-
    decoder_folders = [x for x in Path(f'{Path(__file__).parent}').iterdir()
                       if x.is_dir() and x.name.startswith('DL-')]
    
    for folder in decoder_folders:
        py_decoders_to_rename = [x for x in folder.iterdir()
                            if x.is_file() and x.name.endswith('.py') and x.stem != folder.name]
        
        if len(py_decoders_to_rename) > 1:
            print(f"Multiple files found in folder '{folder.name}':")
            for file in py_decoders_to_rename:
                print(f"  {file.name}")

        for file in py_decoders_to_rename:
            # rename the file to match the parent folder
            new_name = folder.name + '.py'
            file.rename(file.parent / new_name)

if __name__ == "__main__":
    clean_decoder_files()
