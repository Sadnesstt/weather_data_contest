# 기상청 주관 2020년 날씨 빅데이터 콘테스트 데이터분석 부문 장려상 수상작
[최종 보고서](https://drive.google.com/file/d/1Ncsv_r3jz-TYzcPpP9p7UZWI6KtBBn-4/view)
- 진행기간 : 2020.04.27 ~ 2020.06.29
- 주요내용 : 현대 제철 공장의 결로 예측 모델 개발 및 활용방안으로 앱 개발
- 상명 : 장려상(상금 100만원)
- 사용언어 : R, Python
- 공헌한 점

    (1) Domain Knowledge 스터디 : 결로 예측 모델에 대한 논문 및 결로에 영향을 주는 기상현상을 조사, 현대 제철 공장 지형 조사

    (2) 데이터 전처리 : 결측치 및 이상치 제거 / 외부 데이터(예보데이터)를 기존의 결로 데이터와 결합하기 위해 전처리

    (3) 시각화 : 결로 여부와 기상현상의 관계 EDA ⇒ 변수 선택에 활용

    (4) Modeling : 다양한 후보 모델들의 hyperparameter와 변수를 조정하여 성능을 평가, 다양한 모델 중 logistic regression, random forest를 담당

    (5) R shiny 앱 : 결로 예측 모델을 활용하여 결로 발생 확률에 따라 경보 메시지를 전달하는 앱의 UI 구현

- 사용한 Skill

    (1) R을 이용한 데이터 전처리(외부데이터 조인, 결측치 및 이상치 처리)

    (2) Python을 이용한 Logistic Regression, Randomforest, Bagging, Boosting 모델 hyperparameter tuning

    (3) unbalanced dataset 모델의 성능 향상을 위한 연구를 수행(결로 99%, 비결로 1%)

- 결과/성과

    (1) 최종 모델 : LightGBM

    (2) AUC = 0.9869 CSI = 66.36%

    (3) 유의한 변수 : 코일 표면 온도, 예보 이슬점, 공장 내부 습도

    (4) 모델의 성능은 본선 진출 10팀 중 3등

    (5) R shiny 앱 개발(최종 보고서에서 관련 내용 확인 가능)
