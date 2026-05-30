      ******************************************************************
      * SISTEMA : DCOM - I#DCOMTA                                      *
      * TAMANHO : 130 BYTES                                            *
      * ARQUIVO : INTERFACE DCOM X EMPI                                *
      ******************************************************************
       01  TA-REGISTRO.
01  10     05  TA-DT-MOVTO             PIC  X(010).
11  02     05  TA-COD-PROD             PIC  9(003)    COMP-3.
13  01     05  TA-FAM-CONTR            PIC  9(001).
14  05     05  TA-CONTRATO             PIC  9(009)    COMP-3.
19  02     05  TA-SEQ-ATULZ-CONTR      PIC  9(003)    COMP-3.
21  02     05  TA-CTPO-SIT-CONTR       PIC  9(003)    COMP-3.
23  10     05  TA-DCELEB-CONTR         PIC  X(010).
33  03     05  TA-AGENCIA              PIC  9(005)    COMP-3.
36  04     05  TA-CONTA                PIC  9(007)    COMP-3.
40  03     05  TA-CJUNC-DEPDC-NEGOC    PIC  9(005)    COMP-3.
43  02     05  TA-CSPROD               PIC  9(003)    COMP-3.
45  01     05  TA-CMODLD-ATULZ         PIC  9(001).
46  03     05  TA-CIDTFD-ATULZ         PIC  9(005)    COMP-3.
49  02     05  TA-CTPO-GARNT           PIC  9(003)    COMP-3.
51  02     05  TA-CPOSTO-SERVC         PIC  9(003)    COMP-3.
53  02     05  TA-SEGMENTO             PIC  9(003)    COMP-3.
55  03     05  TA-CARTEIRA             PIC  X(003).
58  08     05  TA-QCONTR-DIA           PIC  9(015)    COMP-3.
66  09     05  TA-VALOR                PIC  9(015)V99 COMP-3.
75  08     05  TA-QCONTR-ESTOQUE       PIC  9(015)    COMP-3.
83  09     05  TA-VALOR-ESTOQUE        PIC  9(015)V99 COMP-3.
92  08     05  TA-QTDE-TIT-PARC        PIC  9(015)    COMP-3.
100 08     05  TA-QTDE-TIT-EST         PIC  9(015)    COMP-3.
108 09     05  TA-VLR-VENCD-EST        PIC  9(015)V99 COMP-3.
117 14     05  TA-FILLER               PIC  X(014).
