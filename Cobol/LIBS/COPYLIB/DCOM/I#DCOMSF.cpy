      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSF                                      *
      * TAMANHO : 083 BYTES                                            *
      * ARQUIVO : INTERFACE DCOM X RESP                                *
      ******************************************************************
       01  SF-REGISTRO.
01  05     05  SF-AGENCIA              PIC  9(005).
06  07     05  SF-CONTA                PIC  9(007).
13  34     05  SF-NOME                 PIC  X(034).
47  01     05  SF-MOEDA                PIC  X(001).
48  03     05  SF-CARTEIRA             PIC  X(003).
51  01     05  SF-FILLER               PIC  X(001).
52  03     05  SF-FIRMA                PIC  X(003).
55  07     05  SF-VLR-RESPONS          PIC  9(011)V99 COMP-3.
62  07     05  SF-CONTRATO             PIC  9(007).
69  05     05  SF-DT-VCTO              PIC  9(009)    COMP-3.
74  02     05  SF-QTDE-TTLOS           PIC  9(002).
76  06     05  SF-VLR-PREST            PIC  9(009)V99 COMP-3.
82  02     05  SF-IDENT                PIC  9(002).
