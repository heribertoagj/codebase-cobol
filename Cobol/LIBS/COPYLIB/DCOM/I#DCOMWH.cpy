      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMWH       *
      * ARQUIVO = CHEQUES EXCLUIDOS VIA PROCESSAMENTO CICLICO          *
      * LRECL   = 400 BYTES                 24-07-2009                 *
      *----------------------------------------------------------------*
       01 WH-REGISTRO.
          05 WH-CHV-REG-CHQC8130.
01  03       10  WH-DANO-OPER-DESC               PIC 9(004)    COMP-3.
04  05       10  WH-NSEQ-OPER-DESC               PIC 9(009)    COMP-3.
09  02       10  WH-COD-BCO-CEDEN                PIC 9(003)    COMP-3.
11  03       10  WH-COD-AGEN-CEDEN               PIC 9(005)    COMP-3.
14  07       10  WH-NRO-CTA-CEDEN                PIC 9(013)    COMP-3.
21  03       10  WH-FLIAL-CUST-CEDEN             PIC 9(005)    COMP-3.
24  04       10  WH-NRO-BORD-CUST                PIC 9(007)    COMP-3.
28  04       10  WH-NRO-LOTE-CUST                PIC 9(007)    COMP-3.
32  02       10  WH-COD-BCO-CHEQ                 PIC 9(003)    COMP-3.
34  03       10  WH-COD-AGEN-CHEQ                PIC 9(005)    COMP-3.
37  07       10  WH-NRO-CTA-CHEQ                 PIC 9(013)    COMP-3.
44  04       10  WH-NRO-CHEQ                     PIC 9(007)    COMP-3.
48  09       10  WH-VLR-CHEQ                     PIC 9(015)V99 COMP-3.
57  10       10  WH-DTA-VCTO-CHEQ                PIC X(010).
67  05       10  WH-NRO-CNPJ-CPF-CHEQ            PIC 9(009)    COMP-3.
72  03       10  WH-FLIAL-CPNJ-CPF-CHEQ          PIC 9(005)    COMP-3.
75  02       10  WH-CTRL-CNPJ-CPF-CHEQ           PIC 9(002).
77  50     05 WH-SITUACAO                        PIC X(050).
127 01     05 WH-CTPO-LOTE-OPER                  PIC X(001).
128 02     05 WH-COD-ERRO                        PIC 9(002).
130271     05 FILLER                             PIC X(271).
