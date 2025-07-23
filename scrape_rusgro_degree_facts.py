import requests
import lxml.html
import pandas as pd
import re
import time

def extract_degree_type(title):
    if not title:
        return None

    degree_pattern = r'\b(BSc|BA|BEng|MEng|MSc|MA|PhD|MPhil|LLB|LLM|MB|MBBS|MD|BDS|DDS|PharmD|MSCi|MPharm|DVM|JD)\b'
    match = re.search(degree_pattern, title, flags=re.IGNORECASE)
    
    if match:
        return match.group(1) 
    else:
        return None
    



# def rusgro_degree_facts(url):
#     try:
#         html = requests.get(url, timeout=5).text
#         doc = lxml.html.fromstring(html)

#         # 1. TITLE + TYPE
#         degree_title_results = doc.xpath('//*[@id="main-content"]/div[8]/div/div/div[2]/div/h1/text()')
        
#         if degree_title_results:
#             full_title = degree_title_results[0]  
#             degree_type = extract_degree_type(full_title) 
            
#             # Remove degree type from anywhere in title
#             if degree_type:
#                 clean_title = re.sub(r'\b' + re.escape(degree_type) + r'\b', '', full_title).strip()
#             else:
#                 clean_title = full_title

#             # Remove empty parentheses and extra spaces
#             clean_title = re.sub(r'\s*\(\s*\)\s*', '', clean_title).strip()
#         else:
#             clean_title = None
#             degree_type = None


#         # 2. GET THE SPECIFIC ENTRY REQUIREMENT PARAGRAPH
#         entry_para = doc.xpath('//*[@id="entry"]/section[1]/article/section[1]/section[1]/div[1]/div[2]/p[1]')
#         entry_text = entry_para[0].text_content().strip() if entry_para else ""

#         # 3. FIND A-LEVEL REQUIREMENTS (grade and subject)
#         a_level_grade_req = None
#         a_level_subject_req = None

#         # Grade pattern: e.g. "AAA", "AAB", "ABB", "BBB", "AABB", etc.
#         grade_match = re.search(r'\b([A-C]{3,4})\b', entry_text)
#         if grade_match:
#             a_level_grade_req = grade_match.group(1)
#             # Get the whole sentence containing the grade
#             sentences = re.split(r'(?<=[.!?])\s+', entry_text)
#             for sentence in sentences:
#                 if grade_match.group(1) in sentence:
#                     a_level_subject_req = sentence.strip()
#                     break
#         else:
#             # fallback: just use the whole paragraph if no grade found
#             a_level_subject_req = entry_text

#         return [degree_type, clean_title, a_level_grade_req, a_level_subject_req]


#     except Exception as e:
#         print(f"Error scraping {url}: {e}")
#         return [None, None, None, None]
    

def rusgro_degree_facts(url):
    try:
        html = requests.get(url, timeout=5).text
        doc = lxml.html.fromstring(html)

        # 1. TITLE + TYPE (try multiple XPaths)
        title_xpaths = [
            '//*[@id="main-content"]/div[8]/div/div/div[2]/div/h1/text()',
            '//h1/text()',
            '//title/text()'
        ]
        full_title = None
        for xp in title_xpaths:
            results = doc.xpath(xp)
            if results:
                full_title = results[0].strip()
                break
        degree_type = extract_degree_type(full_title) if full_title else None
        clean_title = re.sub(r'\b' + re.escape(degree_type) + r'\b', '', full_title).strip() if degree_type and full_title else full_title
        if clean_title:
            clean_title = re.sub(r'\s*\(\s*\)\s*', '', clean_title).strip()

        # search for entry reqs
        req_xpaths = [
            '//*[@id="entry"]/section[1]/article/section[1]/section[1]/div[1]/div[2]/p[1]',
            '//*[@id="entry-requirements"]',
            '//*[contains(@class, "entry-requirements")]',
            '//section[contains(@id, "requirements")]',
            '//div[contains(@class, "requirements")]'
        ]
        entry_text = ""
        for xp in req_xpaths:
            entry_para = doc.xpath(xp)
            if entry_para:
                if hasattr(entry_para[0], 'text_content'):
                    entry_text = entry_para[0].text_content().strip()
                else:
                    entry_text = str(entry_para[0]).strip()
                break

        # from entry reqs get a levels (maybe add IB??)
        a_level_grade_req = None
        a_level_subject_req = None

        # Flexible grade regex: "AAA", "A*AA", "AABB", etc.
        grade_match = re.search(r'\b([A*ABCDE]{3,5})\b', entry_text)
        if grade_match:
            a_level_grade_req = grade_match.group(1)
            # Get the whole sentence containing the grade
            sentences = re.split(r'(?<=[.!?])\s+', entry_text)
            for sentence in sentences:
                if a_level_grade_req in sentence:
                    a_level_subject_req = sentence.strip()
                    break
        else:
            # if no grade found just use the whole paragraph 
            a_level_subject_req = entry_text if entry_text else None

        return [degree_type, clean_title, a_level_grade_req, a_level_subject_req]

    except Exception as e:
        print(f"Error scraping {url}: {e}")
        return [None, None, None, None]


# url = "https://www.southampton.ac.uk/courses/mathematics-operational-research-statistics-economics-degree-bsc"
# result = rusgro_degree_facts(url)
# print(result)


df2 = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/rusgro_links.csv")

results = []
for index, row in df2.iterrows():
    url = row['crseurl']
    kis_course_id = row['kiscourseid'] if 'kiscourseid' in row else None  # If present
    
    print(f"Scraping: {url}")
    facts = rusgro_degree_facts(url)
    
    results.append([kis_course_id, url] + facts)
    time.sleep(0.1)

columns = ['kiscourseid', 'url', 'degree_type', 'degree_title', 'a_level_grade_req', 'a_level_subject_req']
final_df = pd.DataFrame(results, columns=columns)

final_df.to_csv('rusgro_degree_facts.csv', index=False)
print("Done!")