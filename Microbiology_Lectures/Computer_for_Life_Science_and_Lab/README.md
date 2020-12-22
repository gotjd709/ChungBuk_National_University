# 프로젝트명: VBA를 통한 유전자 번역

이번 프로젝트는 EXCEL의 VBA(Visual Basic for Application)을 통하여 유전자 서열이 주어졌을 경우 번역(translation)이 일어나도록 하였다. 주요 기능은 다음과 같다.

-----

- 서열 불러오기
  - 저장된 서열 불러오기
  - Genbank number(ex: CP010822)와 유전자 region(ex: 618862..621360)을 입력하여 NCBI 사이트에서 서열 가져오기
- 번역하기
  - 3개씩 묶어 코돈으로 번역하기
  - [IUPAC nucleotide code 고려하기](https://user-images.githubusercontent.com/70703320/102717637-e8e3e600-4326-11eb-8ffe-d337d6c4a8ab.PNG)
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

-----

코드는 다음과 같다.

<pre>
<code>
Public 인터넷익스플로러 As InternetExplorer

Sub 시작()

Dim ans1, ans2 As Integer


    ans1 = MsgBox("저장한 FASTA format 파일을 불러오시겠습니까?" & Chr(10) & "(아니오 입력시 인터넷을 이용해 검색합니다)", vbYesNoCancel)


    If ans1 = "6" Then '"예"를 선택 시 ans1 변수에 저장
    
    
          Call FileRead


    ElseIf ans1 = "7" Then '"아니오"를 선택 시 ans2 변수에 저장
        
        
          ans2 = MsgBox("NCBI에서 gene을 검색하시겠습니까?", vbYesNo)
            
                If ans2 = "6" Then
                    
                       Call 인터넷
                       
                Else

                       Exit Sub
            
                End If
    
    
    Else
    
    
          Exit Sub
        
        
    End If
    
    
Call Translation
    

End Sub
Sub FileRead()

   Dim myFile As String, text As String, textline, test, Blk, seq As String, F As String
   myFile = Application.GetOpenFilename(Filefilter:="text Files(*.txt), *.txt")
   
If myFile = "False" Then
    
   Exit Sub
    
End If

F = FreeFile
Open myFile For Input As #F '파일을 불러 #F로 저장합니다.


    Do While Not EOF(F) ' 파일의 끝을 만날 때까지 반복합니다.
    
        Line Input #F, textline ' 변수로 데이터 행을 읽어들입니다.
        test = test & textline & Chr(10)
        
    Loop
 
    If InStr(test, "") > 0 Then

        MsgBox ("잘못된 파일 형식입니다.")
             
        Exit Sub
            
    End If
        
Close #F
 
Range("B2") = test
 
    '한 줄 제거
    Blk = InStr(Range("B2"), Chr(10))
    seq = Mid(Range("B2"), Blk)
    
    '공백 및 띄어쓰기 제거
    seq1 = Replace(seq, vbCrLf, "")
    seq1 = Replace(seq1, Chr(10), "")
        
Worksheets(1).Range("C2") = seq1

End Sub
Sub 인터넷()

Dim name As String, you As String, Last As String
Dim strA As String
Dim Blk As String
Dim seq, seq1, seq2 As String

name = InputBox("원하는 organism의 genebank code 입력 ")

If name = "" Then
    Exit Sub
End If

you = InputBox("genome에서 원하는 gene sequence 시작 위치 입력 ")

If you = "" Then
    Exit Sub
End If
    
Last = InputBox("genome에서 원하는 gene sequence 마지막 위치 입력 ")

If Last = "" Then
    Exit Sub
End If

    'IE 객체 작성
    Set 인터넷익스플로러 = CreateObject("InternetExplorer.Application") '인터넷 익스플로러 사용
    
        페이지주소 = "https://www.ncbi.nlm.nih.gov/nuccore/" & name & "?report=fasta&from=" & you & "&to=" & Last & "#sequence_" & name '입력할 주소 입력
        인터넷익스플로러.Visible = True '인터넷 창을 보여줌
        인터넷익스플로러.Navigate2 (페이지주소) '페이지 주소로 이동
    
    
        '읽기 완료 대기
        While 인터넷익스플로러.ReadyState <> READYSTATE_COMPLETE Or 인터넷익스플로러.Busy = True 'While Wend 조건이 참일 경우 반복 진행
        
            DoEvents                                                                             '페이지가 로딩 중이면 진행
            Application.Wait (Now + TimeValue("0:00:01")) '페이지를 0.1초 단위로 확인하여 반복
            
        Wend
        Application.Wait (Now + TimeValue("0:00:01"))
  
  
       '엑셀 시트에 검색 결과 출력하기
       For Each element In 인터넷익스플로러.Document.getElementsByClassName("seq gbff") '익스플로러 창에서 class이름이 "seq gbff"로 된 문장을 불러옴
        
            Worksheets(1).Range("B2") = element.innerText '불러온 문장을 워크시트 1번 B2에 입력
            FASTA = element.innerText

       Next element 'element 개수만큼 반복
    
    인터넷익스플로러.Quit '익스플로러 끝내기
    Set 인터넷익스플로러 = Nothing '인터넷익스플로러라는 변수를 비우기

If FASTA = "" Then

    MsgBox ("NCBI에 데이터가 없습니다.")
    Exit Sub
    
End If

    '한 줄 제거
    Blk = InStr(FASTA, Chr(10))
    seq = Mid(FASTA, Blk)
    
    '공백 제거 & 띄어쓰기 제거
    seq1 = Replace(seq, vbCrLf, "")
    seq1 = Replace(seq1, Chr(10), "")
        
Worksheets(1).Range("C2") = seq1

End Sub
Sub Translation()
Dim DNA_seq, RNA_seq, Use, Last, txt As String

    DNA_seq = Range("C2")
    RNA_seq = Replace(DNA_seq, "T", "U")

 Dim Codon As String
 Dim Amino_Acid As String
 Dim Protein_seq As String
 Dim k, s, t As Integer
 
    Protein_seq = ""
          
    For k = 1 To Len(RNA_seq) Step 3
            
        Codon = Mid(RNA_seq, k, 3)
        
        Select Case Codon

        Case Is = "UUU", "UUC", "UUY"
             Amino_Acid = "Phe"
        Case Is = "UUA", "UUG", "UUR", "CUU", "CUC", "CUA", "CUG", "CUR", "CUY", "CUS", "CUW", "CUK", "CUM", "CUB", "CUD", "CUH", "CUV", "CUN"
             Amino_Acid = "Leu"
        Case Is = "AUU", "AUC", "AUA", "AUH", "AUY", "AUW", "AUM"
             Amino_Acid = "Ile"
        Case Is = "AUG"
             Amino_Acid = "Met"
        Case Is = "GUU", "GUC", "GUA", "GUG", "GUR", "GUY", "GUS", "GUW", "GUK", "GUM", "GUB", "GUD", "GUH", "GUV", "GUN"
             Amino_Acid = "Val"
        Case Is = "CCU", "CCC", "CCA", "CCG", "CCR", "CCY", "CCS", "CCW", "CCK", "CCM", "CCB", "CCD", "CCH", "CCV", "CCN"
             Amino_Acid = "Pro"
        Case Is = "ACU", "ACC", "ACA", "ACG", "ACR", "ACY", "ACS", "ACW", "ACK", "ACM", "ACB", "ACD", "ACH", "ACV", "ACN"
             Amino_Acid = "Thr"
        Case Is = "GCU", "GCC", "GCA", "GCG", "GCR", "GCY", "GCS", "GCW", "GCK", "GCM", "GCB", "GCD", "GCH", "GCV", "GCN"
             Amino_Acid = "Ala"
        Case Is = "UAU", "UAC", "UAY"
             Amino_Acid = "Tyr"
        Case Is = "CAU", "CAC", "CAY"
             Amino_Acid = "His"
        Case Is = "CAA", "CAG", "CAR"
             Amino_Acid = "Gln"
        Case Is = "AAU", "AAC", "AAY"
             Amino_Acid = "Asn"
        Case Is = "AAA", "AAG", "AAR"
             Amino_Acid = "Lys"
        Case Is = "GAU", "GAC", "GAY"
             Amino_Acid = "Asp"
        Case Is = "GAA", "GAG", "GAR"
             Amino_Acid = "Glu"
        Case Is = "UGU", "UGC", "UGY"
             Amino_Acid = "Cys"
        Case Is = "UGG"
             Amino_Acid = "Trp"
        Case Is = "CGU", "CGC", "CGA", "CGG", "CGR", "CGY", "CGS", "CGW", "CGK", "CGM", "CGB", "CGD", "CGH", "CGV", "CGH"
             Amino_Acid = "Arg"
        Case Is = "AGU", "AGC", "AGY", "UCU", "UCC", "UCA", "UCG", "UCR", "UCY", "UCS", "UCW", "UCK", "UCM", "UCB", "UCD", "UCH", "UCV", "UCN"
             Amino_Acid = "Ser"
        Case Is = "AGA", "AGG", "AGR"
             Amino_Acid = "Arg"
        Case Is = "GGU", "GGC", "GGA", "GGG", "GGR", "GGY", "GGS", "GGW", "GGK", "GGM", "GGB", "GGD", "GGH", "GGV", "GGH"
             Amino_Acid = "Gly"
        Case Is = "UGA", "UAA", "UAG", "UAR"
             Amino_Acid = "End"
        Case Else
             Amino_Acid = "Unk"
             
        End Select
        
        If Len(Protein_seq) = 0 Then
              Protein_seq = Amino_Acid
        Else
              Protein_seq = Protein_seq & " " & Amino_Acid
        End If
        

    Next k
    
s = 1
t = Len(Protein_seq)
    
    For s = 1 To t Step 100
        Use = Mid(Protein_seq, s, 100)
        Last = Use & vbCrLf
        txt = txt & Last
    Next s
    
Range("C2") = txt


End Sub
Sub 지우기()

Range("B2").MergeArea.ClearContents

Range("C2").MergeArea.ClearContents


End Sub
Sub 저장()

Dim FS, TE, F As String

FS = Application.GetSaveAsFilename(Filefilter:="text Files(*.txt), *.txt")

   If FS = False Then
       MsgBox "취소하였습니다."
       Exit Sub
   End If

TE = Range("C2")
F = FreeFile

   Open FS For Output As #F

       Print #F, TE
   
   Close #F

MsgBox "저장이 완료되었습니다."
 
End Sub
</code>
</pre>
