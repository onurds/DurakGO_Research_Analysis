import pandas as pd
import re
import os
from datetime import datetime


def convert_survey_data(input_file, output_file=None):
    """
    Convert text-based survey responses to numerical codes for analysis.
    Uses category codes approach for multiple-choice questions.

    Parameters:
    -----------
    input_file : str
        Path to the input CSV file with text-based survey responses
    output_file : str, optional
        Path to save the converted CSV file. If None, generates a filename with timestamp.

    Returns:
    --------
    DataFrame with converted numerical values
    """
    # If no output file specified, create one with timestamp
    if output_file is None:
        timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
        file_name, file_ext = os.path.splitext(input_file)
        output_file = f"{file_name}_converted_{timestamp}{file_ext}"

    print(f"Reading from: {input_file}")

    # Read the input CSV file
    try:
        df = pd.read_csv(input_file, encoding='utf-8')
    except UnicodeDecodeError:
        # Try a different encoding if utf-8 fails
        try:
            df = pd.read_csv(input_file, encoding='ISO-8859-9')  # Turkish encoding
        except:
            df = pd.read_csv(input_file, encoding='latin1')  # Fallback encoding

    print(f"Original CSV has {len(df)} rows and {len(df.columns)} columns")

    # Create a new DataFrame with only metadata columns
    result_df = pd.DataFrame()

    # Metadata columns that should not be modified
    metadata_cols = ['No.', 'Response ID', 'Timestamp', 'Finished', 'Survey ID',
                     'Formbricks ID (internal)', 'User ID', 'Notes', 'Tags', 'url',
                     'userAgent - os', 'userAgent - device', 'userAgent - browser']

    # Copy metadata columns to result DataFrame
    for col in metadata_cols:
        if col in df.columns:
            result_df[col] = df[col]

    # Mapping dictionary for exact column names to coded column names
    column_mapping = {
        '1. DurakGO uygulamasını ne kadar süredir kullanıyorsunuz?': 'USE_1',
        '2. DurakGO\'yu ne sıklıkla kullanıyorsunuz?': 'USE_2',
        '3. DurakGO\'yu genellikle ne zaman kullanırsınız? (Geçerli olan tüm seçenekleri işaretleyin)': 'USE_3',
        '4. En çok hangi özellikleri kullanıyorsunuz? (En fazla 3 tane seçin)': 'USE_4',
        '5. DurakGO\'yu çevrimdışı modda (internet bağlantısı olmadan, yeraltında) kullandınız mı?': 'USE_5',
        '6. Çevrimdışı modu kullandıysanız, ne kadar yardımcı oldu?': 'OFF_1',
        '7. Çevrimdışı varış tahminlerini ne kadar doğru buluyorsunuz?': 'OFF_2',
        '8. Çevrimdışı işlevsellik sizin için ne kadar önemli?': 'OFF_3',
        '9. DurakGO\'yu kullanmadan önce, genellikle bir sonraki varış için ne kadar beklemeyi tahmin ediyordunuz?': 'WAIT_1',
        '10. Şimdi DurakGO ile, genellikle ne kadar beklemeyi tahmin ediyorsunuz?': 'WAIT_2',
        '11. DurakGO bekleme deneyiminizi nasıl etkiledi?': 'WAIT_3',
        '12. Varış zamanı için DurakGO\'yu kullandığınızda metro beklerken hisleriniz genel olarak nasıl etkileniyor? (bekleme hissiyatı)': 'WAIT_4',
        '13. DurakGO uzun bir bekleme süresi gösterdiğinde genellikle ne yaparsınız? (Geçerli olan tüm seçenekleri işaretleyin)': 'WAIT_5',
        '14. DurakGO\'yu kullanmak Ankara\'nın metro/raylı sistemini kullanma sıklığınızı değiştirdi mi?': 'IMP_1',
        '15. DurakGO\'yu metro sistemi için EGO Cep\'te uygulamasıyla nasıl karşılaştırırsınız?': 'IMP_2',
        '16. 1-5 arası bir ölçekte, Ankara\'nın metro/raylı sistemini genel olarak nasıl değerlendirirsiniz?': 'IMP_3',
        '17. 1-5 arası bir ölçekte, DurakGO uygulamasını genel olarak nasıl değerlendirirsiniz?': 'SAT_1',
        '18. DurakGO\'nun en çok hangi yönlerini beğeniyorsunuz? (En fazla 3 tane seçin)': 'SAT_2',
        '19. DurakGO\'da ne iyileştirilebilir? (Geçerli olan tüm seçenekleri işaretleyin)': 'SAT_3',
        '20. DurakGO\'ya hangi özelliklerin eklenmesini istersiniz? (Geçerli olan tüm seçenekleri işaretleyin)': 'SAT_4',
        '21. DurakGO\'yu başkalarına tavsiye eder misiniz?': 'SAT_5',
        '22. Ankara\'nın metro/raylı sistemini ne sıklıkla kullanıyorsunuz?': 'DEM_1',
        '23. Ankara\'daki birincil (en fazla) ulaşım şekliniz nedir?': 'DEM_2',
        '24. En sık hangi hatları kullanıyorsunuz? (Geçerli olan tüm seçenekleri işaretleyin)': 'DEM_3',
        '25. Metro/raylı sistemi kullanırken genel olarak seyahat amacınız nedir?': 'DEM_4',
        '26. Yaş grubunuz nedir?': 'DEM_5',
        '27. Cinsiyetiniz nedir?': 'DEM_6',
        '28. DurakGO\'yu iyileştirmek için başka yorumlarınız veya önerileriniz var mı?': 'TEXT_1'
    }

    # Process each question
    for col_name, new_col_name in column_mapping.items():
        # Find the closest matching column in the dataframe
        matching_cols = [c for c in df.columns if col_name in c]

        if not matching_cols:
            print(f"Warning: Could not find a column matching '{col_name}' in the CSV")
            continue

        # Use the first matching column
        actual_col_name = matching_cols[0]

        # Single-choice questions
        if new_col_name == 'USE_1':
            coding = {
                "1 haftadan az": 1,
                "1-4 hafta": 2,
                "1-3 ay": 3,
                "3 aydan fazla": 4
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'USE_2':
            coding = {
                "Günde birden çok kez": 1,
                "Günde bir kez": 2,
                "Haftada birkaç kez": 3,
                "Haftada bir kez veya daha az": 4
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'USE_3':
            # Multiple-choice question with category codes
            options = {
                "Bulunduğum yerden ayrılmadan önce": 1,
                "İstasyona giderken": 2,
                "İstasyonda": 3,
                "Sefer aksamaları sırasında": 4,
                "Gezileri önceden planlamak için": 5
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "3_" + ",".join([str(options[opt]) for opt in options if isinstance(x, str) and opt in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'USE_4':
            # Multiple-choice question with category codes
            options = {
                "Sonraki varış zamanlarını kontrol etme": 1,
                "En yakın istasyonları bulma": 2,
                "İstasyon haritasını görüntüleme": 3,
                "Belirli istasyonları arama": 4,
                "İstasyonlardaki her iki yönü de görüntüleme": 5
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "4_" + ",".join([str(options[opt]) for opt in options if isinstance(x, str) and opt in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'USE_5':
            coding = {
                "Evet sık sık": 1,
                "Evet ara sıra": 2,
                "Hayır kullandığım her zaman internet bağlantım var": 3,
                "Hayır çevrimdışı çalıştığını bilmiyordum": 4
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'OFF_1':
            coding = {
                "Çok yardımcı oldu": 5,
                "Biraz yardımcı oldu": 4,
                "Pek yardımcı olmadı": 3,
                "Hiç yardımcı olmadı": 2,
                "Kullanmadım": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'OFF_2':
            coding = {
                "Çoğunlukla doğru": 5,
                "Kısmen doğru": 4,
                "Ne doğru ne yanlış": 3,
                "Kısmen yanlış": 2,
                "Çoğunlukla yanlış": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'OFF_3':
            coding = {
                "Çok Gerekli - DurakGO'yu bu yüzden kullanıyorum": 4,
                "Çok önemli": 3,
                "Kısmen önemli": 2,
                "Önemli değil": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'WAIT_1':
            coding = {
                "3 dakikadan az": 1,
                "3-5 dakika": 2,
                "6-10 dakika": 3,
                "11-15 dakika": 4,
                "15 dakikadan fazla": 5
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'WAIT_2':
            coding = {
                "3 dakikadan az": 1,
                "3-5 dakika": 2,
                "6-10 dakika": 3,
                "11-15 dakika": 4,
                "15 dakikadan fazla": 5
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'WAIT_3':
            coding = {
                "Beklemeyi çok daha kısa hissettiriyor": 5,
                "Beklemeyi biraz daha kısa hissettiriyor": 4,
                "Etkisi yok": 3,
                "Beklemeyi biraz daha uzun hissettiriyor": 2,
                "Beklemeyi çok daha uzun hissettiriyor": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'WAIT_4':
            coding = {
                "Çok daha rahatlamış": 5,
                "Biraz daha rahatlamış": 4,
                "Fark yok": 3,
                "Biraz daha endişeli": 2,
                "Çok daha endişeli": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'WAIT_5':
            # Multiple-choice question with category codes
            options = {
                "İstasyona gidiş zamanımı ona göre ayarlarım": 1,
                "Farklı bir istasyon seçerim": 2,
                "Farklı bir ulaşım şekli kullanırım": 3,
                "Beklerken verimli bir şeyler yaparım": 4,
                "Farklı bir rota kullanırım": 5,
                "Davranışımda değişiklik olmaz": 6
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "5_" + ",".join([str(options[opt]) for opt in options if isinstance(x, str) and opt in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'IMP_1':
            coding = {
                "Çok daha sık kullanıyorum": 5,
                "Biraz daha sık kullanıyorum": 4,
                "Değişiklik yok": 3,
                "Biraz daha az kullanıyorum": 2,
                "Çok daha az kullanıyorum": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'IMP_2':
            coding = {
                "DurakGO çok daha iyi": 5,
                "DurakGO biraz daha iyi": 4,
                "Hemen hemen aynılar": 3,
                "EGO Cep'te daha iyi": 2,
                "EGO Cep'te'yi kullanmadım": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'IMP_3':
            coding = {
                "1 (Kötü)": 1,
                "2": 2,
                "3": 3,
                "4": 4,
                "5 (Mükemmel)": 5
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'SAT_1':
            coding = {
                "1 (Kötü)": 1,
                "2": 2,
                "3": 3,
                "4": 4,
                "5 (Mükemmel)": 5
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'SAT_2':
            # Multiple-choice question with category codes
            options = {
                "Çevrimdışı işlevsellik": 1,
                "Kullanıcı dostu arayüz": 2,
                "Tahminlerin doğruluğu": 3,
                "Favori istasyonlar özelliği": 4,
                "En yakın istasyon özelliği": 5,
                "Metro/raylı sisteme özel odaklanma": 6,
                "Arama işlevselliği": 7
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "2_" + ",".join([str(options[opt]) for opt in options if isinstance(x, str) and opt in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'SAT_3':
            # Multiple-choice question with category codes
            options = {
                "Uygulama hızı/performansı": 1,
                "Kullanıcı arayüzü": 2,
                "Varış doğruluğu": 3,
                "Ek özellikler": 4,
                "Pil kullanımı": 5,
                "İstasyon bilgileri": 6,
                "Diğer (lütfen son soruda belirtin)": 7
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "3_" + ",".join([str(options[opt]) for opt in options if isinstance(x, str) and opt in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'SAT_4':
            # Multiple-choice question with category codes
            options = {
                "Yolculuk planlama": 1,
                "Ücret bilgisi": 2,
                "Diğer ulaşım modlarıyla entegrasyon": 3,
                "Sefer aksaması uyarı bildirimleri": 4,
                "Diğer (lütfen son soruda belirtin)": 5
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "4_" + ",".join([str(options[opt]) for opt in options if isinstance(x, str) and opt in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'SAT_5':
            coding = {
                "Kesinlikle evet": 5,
                "Muhtemelen evet": 4,
                "Emin değilim": 3,
                "Muhtemelen hayır": 2,
                "Kesinlikle hayır": 1
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'DEM_1':
            coding = {
                "Haftada 5 veya daha fazla gün": 1,
                "Haftada 2-4 gün": 2,
                "Haftada bir kez": 3,
                "Ayda 1-3 kez": 4,
                "Ayda birden az": 5
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'DEM_2':
            coding = {
                "Metro/Raylı Sistem": 1,
                "Otobüs": 2,
                "Özel araç": 3,
                "Taksi": 4,
                "Yürüme": 5,
                "Bisiklet": 6
            }

            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'DEM_3':
            # Multiple-choice question with category codes
            options = {
                "Ankaray (A1)": 1,
                "Sincan-Çayyolu (M1)": 2,
                "Keçiören Metrosu (M4)": 3,
                "Başkentray (B1)": 4
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "3_" + ",".join([str(options[key]) for key in options if isinstance(x, str) and key in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'DEM_4':
            # Multiple-choice question with category codes
            options = {
                "İşe gidip gelme": 1,
                "Okula/üniversiteye gidip gelme": 2,
                "Alışveriş": 3,
                "Sosyal/eğlence etkinlikleri": 4
            }

            result_df[new_col_name] = df[actual_col_name].apply(
                lambda x: "4_" + ",".join([str(options[key]) for key in options if isinstance(x, str) and key in x])
                if pd.notna(x) else None
            )

        elif new_col_name == 'DEM_5':
            coding = {
                "18 yaş altı": 1,
                "18-24": 2,
                "25-34": 3,
                "35-44": 4,
                "45-54": 5,
                "55-64": 6,
                "65 yaş ve üzeri": 7,
                "Belirtmek istemiyorum": 8
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'DEM_6':
            coding = {
                "Erkek": 1,
                "Kadın": 2,
                "Belirtmek istemiyorum": 3,
                "Diğer": 4
            }
            result_df[new_col_name] = df[actual_col_name].map(coding)

        elif new_col_name == 'TEXT_1':
            # Keep as text field for qualitative analysis
            result_df[new_col_name] = df[actual_col_name]

    # Reorder columns: first metadata, then converted columns
    column_order = (
            metadata_cols +
            [col for col in result_df.columns if col.startswith('USE_')] +
            [col for col in result_df.columns if col.startswith('OFF_')] +
            [col for col in result_df.columns if col.startswith('WAIT_')] +
            [col for col in result_df.columns if col.startswith('IMP_')] +
            [col for col in result_df.columns if col.startswith('SAT_')] +
            [col for col in result_df.columns if col.startswith('DEM_')] +
            [col for col in result_df.columns if col.startswith('TEXT_')]
    )

    # Filter to keep only columns that exist in result_df
    ordered_cols = [col for col in column_order if col in result_df.columns]

    # Reorder DataFrame columns
    final_df = result_df[ordered_cols]

    # Save to CSV
    final_df.to_csv(output_file, index=False, encoding='utf-8')
    print(f"Converted data saved to: {output_file}")

    return final_df


if __name__ == "__main__":
    # Get input file path from user
    input_file = input("Enter the path to the CSV file: ")

    # Generate output filename
    file_name, file_ext = os.path.splitext(input_file)
    output_file = f"{file_name}_converted{file_ext}"

    # Convert the data
    convert_survey_data(input_file, output_file)