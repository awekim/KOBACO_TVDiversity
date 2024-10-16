# KOBACO_project

- This repository contains samples codes for analysing KOBACO data
- swd: 실시간TV 개인시청기록
  - HouseholdID
  - IndividualID： 손님, Baby(4세미만), 버튼입력 없이 시청한 경우 50~99번으로 표기
  - Channel
  - StartTime
  - EndTime
  - TV
  - AudienceType
  - Date
- weh: 가구 가중치
  - HouseholdID
  - NWeight
  - Reg
  - Weight: Weight of the Household in the day
  - Date
- dem: 개인 가중치
  - HouseholdID
  - IndividualID
  - IndWeight: Individual's Weight in the day
  - Reg
  - Sex: 1-남자, 2-여자
  - Job
  - Household: 1-1인, 2-2인, 3-3인, 4-4인, 5-5인이상
  - Teen:
    4-9세 남자	1
    4-9세 여자	2
    10대 남자	3
    10대 여자	4
    20대 남자	5
    20대 여자	6
    30대 남자	7
    30대 여자	8
    40대 남자	9
    40대 여자	A
    50대 남자	B
    50대 여자	C
    60세이상 남자	D
    60세이상 여자	E
  - Machines: 0-TV없음, 1-1대, 2-2대이상
  - Cable: 8-케이블 비가입, 9-디지털케이블, A-아날로그케이블, B-8VSB케이블
  - SkyLife: F-HD가입, G-OTS가입, H-스카이라이프 비가입
  - IPTV: E-실시간패키지, F-VOD패키지, G-IPTV 비가입
  - Date
