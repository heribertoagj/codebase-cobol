      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSB                                      *
      * TAMANHO : 0180 BYTES                                           *
      * ARQUIVO : CONTABIL (GERADO PELO PROGRAMA DCOM9050-TRATA        *
      *           INTERFACE COM RISC)                                *
      ******************************************************************
       01  SB-REGISTRO.
  1  4     05 SB-CCTRO-CUSTO-ROT      PIC  X(4).
  5  3     05 SB-CAG-BCRIA-CLI        PIC S9(5)V USAGE COMP-3.
  8  7     05 SB-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
 15  3     05 SB-CCART                PIC  X(3).
 18  9     05 SB-CCONTR               PIC S9(17)V USAGE COMP-3.
 27  3     05 SB-CEMPR                PIC S9(5)V  USAGE COMP-3.
 30  2     05 SB-CGRUPO-CONTAB        PIC S9(3)V  USAGE COMP-3.
 32  2     05 SB-SGRUPO-CONTAB        PIC S9(3)V  USAGE COMP-3.
 34  1     05 SB-CSIT-PARC            PIC  9(1).
 35  1     05 SB-CTPO-REG             PIC  9(1).
 36  2     05 SB-CGRUPO-CTA           PIC S9(3)V USAGE COMP-3.
 38  2     05 SB-SGRUPO-CTA           PIC S9(3)V USAGE COMP-3.
 40  3     05 SB-CAG-CONTAB           PIC S9(5)V USAGE COMP-3.
 43  1     05 SB-CATIVO-PASSIVO       PIC  9(1).
 44  2     05 SB-CMES-BASE            PIC  9(2).
 46  4     05 SB-CANO-BASE            PIC  9(4).
 50  5     05 SB-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
 55  3     05 SB-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
 58  2     05 SB-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
 60  1     05 SB-CTPO-PESSOA-CLI      PIC  X(1).
 61 40     05 SB-INOME-CLI            PIC  X(40).
101  2     05 SB-CDIST-VECTO          PIC  9(2).
103  9     05 SB-VALOR                PIC S9(15)V9(2) USAGE COMP-3.
112  3     05 SB-CGARNT-PLCC          PIC S9(4) USAGE COMP-3.
115  5     05 SB-CCNPJ-CPF-COOBRIG    PIC S9(9) USAGE COMP-3.
120  3     05 SB-CFLIAL-CNPJ-COOBRIG  PIC S9(5) USAGE COMP-3.
123  2     05 SB-CCTRL-CNPJ-CPF-COOBRIG PIC S9(2)USAGE COMP-3.
125  2     05 SB-CLASS-OPER           PIC  9(2).
127  2     05 SB-CNATU-OPER           PIC  9(2).
129  4     05 SB-CORIG-REC-OPER       PIC  9(4).
133  2     05 SB-CTPO-TXA-REF         PIC  9(2).
135  3     05 SB-VAR-CAMBIAL          PIC  9(3).
138  6     05 SB-VTXA-ENCARGOS        PIC S9(4)V9(7) USAGE COMP-3.
144  2     05 SB-CCARAC-ESP-OPER      PIC  9(2).
146  5     05 SB-DT-INIC-OPER         PIC S9(8)V  USAGE COMP-3.
151  5     05 SB-DT-VECTO-PARC        PIC S9(8)V  USAGE COMP-3.
156  5     05 SB-DT-VECTO-ULT-PARC    PIC S9(8)V  USAGE COMP-3.
161  6     05 SB-FILLER-1             PIC  X(6).
167 14     05 SB-FILLER-2             PIC  X(14).
