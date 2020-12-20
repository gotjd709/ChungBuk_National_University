# 프로젝트명: VBA를 통한 유전자 번역

이번 프로젝트는 EXCEL의 VBA(Visual Basic for Application)을 통하여 유전자 서열이 주어졌을 경우 번역(translation)이 일어나도록 하였다. 주요 기능은 다음과 같다.

-----

- 서열 불러오기
  - 저장된 서열 불러오기
  - Genbank number(ex: CP010822)와 유전자 region(ex: 618862..621360)을 입력하여 NCBI 사이트에서 서열 가져오기
- 번역하기
  - 3개씩 묶어 코돈으로 번역하기
  - IUPAC nucleotide code 고려하기
- 저장하기
  - 번역된 서열 저장하기
- 지우기
  - 모든 서열 지우기
  
-----

알고리즘은 다음과 같다.

- Main sub()

![Main](C:/Users/ay190130/Desktop/Final_Project/5.png)

- File Read sub()

![File](C:/Users/ay190130/Desktop/Final_Project/6.png)

- 인터넷 검색 sub()

![인터넷](C:/Users/ay190130/Desktop/Final_Project/7.png)

- Translation sub()

![Translation](C:/Users/ay190130/Desktop/Final_Project/8.png)

- save sub()

![save](C:/Users/ay190130/Desktop/Final_Project/9.png)

- clear sub()

![clear](C:/Users/ay190130/Desktop/Final_Project/10.png)
