# CSE4050_Programming_Language 과제 구현

이 리포지토리에는 ‘프로그래밍언어’ 과목의 네 가지 Lab 과제가 포함되어 있다. 각 Lab는 F#으로 작성된 skeleton code를 바탕으로, 주어진 기능을 구현하는 방식으로 진행된다.

```
├── Lab1_Warmup/      # 간단한 F# 함수 구현 문제 (7개)
├── Lab2_CMinus/      # C- 언어 인터프리터 (포인터 비포함)
├── Lab2_CMinusPtr/   # C- 언어 확장: 포인터 추가 인터프리터
├── Lab3_FMinus/      # F- 언어 인터프리터
├── Lab4_FMinusType/  # F- 언어 정적 타입 추론기
└── README.md         # 과제 구현 요약
```

---

## Lab1: Warm-Up Exercise

* **문제 수**: P1\~P7, 각 문제마다 F# 함수 하나 구현
* **핵심 구현**:

  * 리스트 뒤집기, 합계, 조건 연산자, 기타 기본 함수
  * 재귀와 패턴 매칭 활용
  * 주어진 제약(내장 함수/연산자 사용 금지) 준수
* **컴파일 & 실행**:

  ```bash
  cd Lab1_Warmup/P<문제번호>
  dotnet build -o out
  ./out/P<문제번호>
  ```
* **자가 채점**: `check.py` 스크립트로 테스트 통과 여부 확인

---

## Lab2: C- Interpreter

### CMinus (비포인터 버전)

* **구현 파일**: `CMinus.fs`
* **주요 기능**:

  * AST 정의된 `Exp`, `Stmt`에 대해 `evalExp`와 `exec` 함수 완성
  * 산술/논리 연산, 변수 할당, 시퀀스, 조건문, 반복문, NOP 처리
  * 정의되지 않은 실행 경로에서 `UndefinedSemantics` 예외 발생
* **실행**:

  ```bash
  cd Lab2_CMinus
  dotnet build -o out
  ./out/CMinus testcase/tc-1
  ```

### CMinusPtr (포인터 확장 버전)

* **구현 파일**: `CMinusPtr.fs`
* **추가 기능**:

  * L-value 평가(`&x`, `*e`) 및 포인터 할당 처리
  * 기존 CMinus 코드 재사용 가능
* **실행**:

  ```bash
  cd Lab2_CMinusPtr
  dotnet build -o out
  ./out/CMinusPtr testcase/tc-1
  ```

---

## Lab3: F- Interpreter

* **구현 파일**: `FMinus.fs`
* **핵심 구현**:

  * 전체 프로그램을 하나의 `Exp`로 보고 `evalExp` 함수로 평가
  * 숫자, 불리언, 산술/비교 연산, 조건식, `let` 바인딩, 함수 정의(익명/재귀), 함수 호출 지원
  * 환경(`Env`) 기반 변수 바인딩 및 클로저 처리
* **실행**:

  ```bash
  cd Lab3_FMinus
  dotnet build -o out
  ./out/FMinus testcase/tc-1
  ```

---

## Lab4: F- Type System

* **구현 파일**: `TypeSystem.fs`
* **핵심 구현**:

  * 주어진 타입 환경(`TypeEnv`)과 typing rules에 따라 `Type.infer` 함수 구현
  * 산술/비교/조건/함수(`fun`, `let`, `let rec`) 표현식의 타입 추론
  * 연산자 오버로드(`=`, `<>`) 처리
  * 타입 불일치 시 `TypeError` 예외 발생
* **실행**:

  ```bash
  cd Lab4_FMinusType
  dotnet build -o out
  ./out/FMinusType testcase/tc-1
  ```

---

