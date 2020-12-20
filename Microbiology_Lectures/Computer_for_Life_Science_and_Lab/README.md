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

- Main sub

![Main](https://user-images.githubusercontent.com/70703320/102717466-d917d200-4325-11eb-9b0e-bd5da816b559.PNG)

- File Read sub

![File](https://user-images.githubusercontent.com/70703320/102717506-26943f00-4326-11eb-9339-d90755763a9b.PNG)

- 인터넷 검색 sub

![인터넷](https://user-images.githubusercontent.com/70703320/102717547-69561700-4326-11eb-83fb-7370bd34d848.PNG)

- Translation sub

![Translation](https://user-images.githubusercontent.com/70703320/102717562-8985d600-4326-11eb-8b9b-4b837a43d083.PNG)

- save sub

![save](https://user-images.githubusercontent.com/70703320/102717579-9c98a600-4326-11eb-800a-4bf1320bc15c.PNG)

- clear sub

![clear](https://user-images.githubusercontent.com/70703320/102717587-aa4e2b80-4326-11eb-8018-46ab76b07d88.PNG)
