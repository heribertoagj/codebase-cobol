      ******************************************************************
      * SISTEMA : DCOM - I#DCOMWB                                      *
      * TAMANHO : 0260 BYTES                                           *
      * ARQUIVO : ARQUIVO INTERFACE SISTEMA WARE                       *
      * CRIACAO : 30/06/2008                ATUALIZADO : 30/06/2008    *
      ******************************************************************
       01  WARE-REGISTRO.
 1  68     05  WARE-CHAVE-SUMARIZADA.
 1  10       10  WARE-CHAVE-CEDENTE.
 1   3         15  WARE-AGENCIA              PIC  9(05)       COMP-3.
 4   4         15  WARE-CONTA                PIC  9(07)       COMP-3.
 8   3         15  WARE-CARTEIRA             PIC  X(03).
11   1       10  WARE-TAXA                   PIC  X.
12   1       10  WARE-TP-CONEXAO             PIC  X.
13  56       10  WARE-DADOS-SACADO.
13   5         15  WARE-NRO-CGCCPF-SACADO    PIC  9(09)       COMP-3.
18   3         15  WARE-FIL-CGCCPF-SACADO    PIC  9(05)       COMP-3.
21   2         15  WARE-CTR-CGCCPF-SACADO    PIC  9(03)       COMP-3.
23  40         15  WARE-NOME-SACADO          PIC  X(40).
63   6         15  WARE-CEP.
63   3           20 WARE-NUM-CEP-SACADO      PIC  9(05)       COMP-3.
66   3           20 WARE-SUF-CEP-SACADO      PIC  X(03).
           05  WARE-TITULOS-VENCIDOS.
69   5       10  WARE-QTD-VENC-ATE05         PIC  9(09)       COMP-3.
74   9       10  WARE-VLR-VENC-ATE05         PIC S9(15)V99    COMP-3.
83   6       10  WARE-PRZ-VENC-ATE05         PIC  9(11)       COMP-3.
89   5       10  WARE-QTD-VENC-06A10         PIC  9(09)       COMP-3.
94   9       10  WARE-VLR-VENC-06A10         PIC S9(15)V99    COMP-3.
103  6       10  WARE-PRZ-VENC-06A10         PIC  9(11)       COMP-3.
109  5       10  WARE-QTD-VENC-11A30         PIC  9(09)       COMP-3.
114  9       10  WARE-VLR-VENC-11A30         PIC S9(15)V99    COMP-3.
123  6       10  WARE-PRZ-VENC-11A30         PIC  9(11)       COMP-3.
129  5       10  WARE-QTD-VENC-ACI30         PIC  9(09)       COMP-3.
134  9       10  WARE-VLR-VENC-ACI30         PIC S9(15)V99    COMP-3.
143  6       10  WARE-PRZ-VENC-ACI30         PIC  9(11)       COMP-3.
           05  WARE-TITULOS-A-VENCER.
149  5       10  WARE-QTD-AVENC-ATE05        PIC  9(09)       COMP-3.
154  9       10  WARE-VLR-AVENC-ATE05        PIC S9(15)V99    COMP-3.
163  6       10  WARE-PRZ-AVENC-ATE05        PIC  9(11)       COMP-3.
169  5       10  WARE-QTD-AVENC-06A10        PIC  9(09)       COMP-3.
174  9       10  WARE-VLR-AVENC-06A10        PIC S9(15)V99    COMP-3.
183  6       10  WARE-PRZ-AVENC-06A10        PIC  9(11)       COMP-3.
189  5       10  WARE-QTD-AVENC-11A30        PIC  9(09)       COMP-3.
194  9       10  WARE-VLR-AVENC-11A30        PIC S9(15)V99    COMP-3.
203  6       10  WARE-PRZ-AVENC-11A30        PIC  9(11)       COMP-3.
209  5       10  WARE-QTD-AVENC-31A60        PIC  9(09)       COMP-3.
214  9       10  WARE-VLR-AVENC-31A60        PIC S9(15)V99    COMP-3.
223  6       10  WARE-PRZ-AVENC-31A60        PIC  9(11)       COMP-3.
229  5       10  WARE-QTD-AVENC-ACI60        PIC  9(09)       COMP-3.
234  9       10  WARE-VLR-AVENC-ACI60        PIC S9(15)V99    COMP-3.
243  6       10  WARE-PRZ-AVENC-ACI60        PIC  9(11)       COMP-3.
249  5     05  WARE-DATA-MOVTO               PIC  9(09)       COMP-3.
254  7     05  FILLER                        PIC  X(07).
