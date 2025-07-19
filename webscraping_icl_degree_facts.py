import requests
import lxml.html
import pandas as pd


# URL  = "https://www.imperial.ac.uk/study/courses/undergraduate/computing-software-engineering-meng/"
# html = requests.get(URL, timeout=15).text
# doc  = lxml.html.fromstring(html)

# degree_type     = doc.xpath('//*[@id="content"]/div[3]/div/section[2]/div/ul/li[1]/ul/li/h4/text()')[0]
# grade_req       = doc.xpath('//*[@id="course-entry-1"]/div/div/div/div[1]/div[1]/p/strong/text()')[0]
# offer_rate_text = doc.xpath('//*[@id="course-entry-1"]/div/div/div/div[2]/div[2]/p[1]/text()')[0]

# # ---------- subject requirements (first *two* bullets) ----------
# bullets = doc.xpath('//*[@id="course-entry-1"]//ul/li')
# req_bullets = [
#     li.xpath('normalize-space(string(.))')
#     for li in bullets[:2]          
# ]
# subject_requirements = " ; ".join(req_bullets)

# # ---------- print results ----------
# print("Degree type:        ", degree_type)
# print("Grade requirement:  ", grade_req)
# print("Offer‑rate text:    ", offer_rate_text)
# print("Subject requirements:", subject_requirements)

# automate!


# def icl_degree_facts(URL):
#     import requests, lxml.html

#     html = requests.get(URL, timeout=15).text
#     doc = lxml.html.fromstring(html)

#     degree_title = doc.xpath('//*[@id="content"]/section[1]/div[2]/div/div/h1/text()')[0]
#     degree_type = doc.xpath('//*[@id="content"]/div[3]/div/section[2]/div/ul/li[1]/ul/li/h4/text()')[0]
#     grade_req = doc.xpath('//*[@id="course-entry-1"]/div/div/div/div[1]/div[1]/p/strong/text()')[0]

#     bps = doc.xpath('//*[@id="course-entry-1"]//ul/li')
#     req_bps = [li.xpath('normalize-space(string(.))') for li in bps[:2]]
#     subject_reqs = " ; ".join(req_bps)

#     icl_degree_fact = [degree_title, degree_type, grade_req, subject_reqs]
#     print(icl_degree_fact)


# url1 = "https://www.imperial.ac.uk/study/courses/undergraduate/mathematics-statistics-finance/"
# icl_degree_facts(url1)







# def icl_degree_facts(URL):
#     try:
#         html = requests.get(URL, timeout=15).text
#         doc = lxml.html.fromstring(html)

#         degree_title = doc.xpath('//*[@id="content"]/section[1]/div[2]/div/div/h1/text()')[0]
#         degree_type = doc.xpath('//*[@id="content"]/div[3]/div/section[2]/div/ul/li[1]/ul/li/h4/text()')[0]
#         grade_req = doc.xpath('//*[@id="course-entry-1"]/div/div/div/div[1]/div[1]/p/strong/text()')[0]

#         bps = doc.xpath('//*[@id="course-entry-1"]//ul/li')
#         req_bps = [li.xpath('normalize-space(string(.))') for li in bps[:2]]
#         subject_reqs = " ; ".join(req_bps)

#         return [degree_type, degree_title, grade_req, subject_reqs]
#     except:
#         return [None, None, None, None]

# # Read CSV
# df = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/imperial_discuni.csv")

# # Apply function to each URL
# results = []
# for url in df['crseurl']:
#     print(f"Scraping: {url}")
#     facts = icl_degree_facts(url)
#     results.append([url] + facts)
#     time.sleep(1)

# # Create dataframe
# columns = ['url', 'degree_type', 'degree_title', 'grade_req', 'subject_reqs']
# final_df = pd.DataFrame(results, columns=columns)

# # Save
# final_df.to_csv('imperial_results.csv', index=False)
# print("Done!")

### v2

def icl_degree_facts(URL):
    try:
        html = requests.get(URL, timeout=15).text
        doc = lxml.html.fromstring(html)


        degree_title_results = doc.xpath('//*[@id="content"]/section[1]/div[2]/div/div/h1/text()')
        degree_title = degree_title_results[0] if degree_title_results else None
        
        degree_type_results = doc.xpath('//*[@id="content"]/div[3]/div/section[2]/div/ul/li[1]/ul/li/h4/text()')
        degree_type = degree_type_results[0] if degree_type_results else None

        # a levels
        a_level_grade_results = doc.xpath('//*[@id="course-entry-1"]/div/div/div/div[1]/div[1]/p/strong/text()')
        a_level_grade_req = a_level_grade_results[0] if a_level_grade_results else None
        
        bps1 = doc.xpath('//*[@id="course-entry-1"]//ul/li')
        req_bps1 = [li.xpath('normalize-space(string(.))') for li in bps1[:2]]
        a_level_subject_reqs = "; ".join(req_bps1) if req_bps1 else None

        # ib 
        ib_grade_results = doc.xpath('//*[@id="course-entry-2"]/div/div[1]/div/div[1]/div[1]/p/strong/text()')
        ib_grade_req = ib_grade_results[0] if ib_grade_results else None
        
        bps2 = doc.xpath('//*[@id="course-entry-2"]//ul/li')
        req_bps2 = [li.xpath('normalize-space(string(.))') for li in bps2[:3]]
        ib_subject_req = "; ".join(req_bps2).replace('\xa0', ' ') if req_bps2 else None

        # return

        return [degree_type, degree_title, a_level_grade_req, a_level_subject_reqs, ib_grade_req, ib_subject_req]
    
    except Exception as e:
        print(f"Error {e}")  
        return [None, None, None, None, None, None]
    
# url = "https://www.imperial.ac.uk/study/courses/undergraduate/chemical-engineering/"

# result = icl_degree_facts(url)
# print(result)

# Read CSV
df = pd.read_csv("https://raw.githubusercontent.com/Danjones-DJ/Degree-Matchmaker_DJ/refs/heads/main/imperial_discuni.csv")

# Apply function to each URL
results = []
for url in df['crseurl']:
    print(f"Scraping: {url}")
    facts = icl_degree_facts(url)
    results.append([url] + facts)
    time.sleep(1)

# Create dataframe
columns = ['url', 'degree_type', 'degree_title', 'a_level_grade_req', 'a_level_subject_reqs', 'ib_grade_req', 'ib_subject_req']
final_df = pd.DataFrame(results, columns=columns)

# Save
final_df.to_csv('imperial_results.csv', index=False)
print("Done!")