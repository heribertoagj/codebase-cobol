      ******************************************************************
      * SISTEMA : DCOM - I#DCOMTX                                      *
      * TAMANHO : 180 BYTES                                            *
      * ARQUIVO : INTERFACE DCOM X RISC                                *
      ******************************************************************
       01  TX-REGISTRO.
01  04     05  TX-CENTRO-CUSTO            PIC  X(04).
05  03     05  TX-AGENCIA                 PIC  9(05)       COMP-3.
08  07     05  TX-CONTA                   PIC  9(13)       COMP-3.
15  03     05  TX-CARTEIRA                PIC  X(03).
18  09     05  TX-CONTRATO                PIC  9(17)       COMP-3.
27  03     05  TX-EMPRESA                 PIC  9(05)       COMP-3.
30  02     05  TX-GRUPO                   PIC  9(03)       COMP-3.
32  02     05  TX-SUBGRUPO                PIC  9(03)       COMP-3.
34  01     05  TX-SITUACAO                PIC  9(01).
35  01     05  TX-TIPREG                  PIC  9(01).
36  02     05  TX-GRUPO-CC                PIC  9(03)       COMP-3.
38  02     05  TX-SUBGRUPO-CC             PIC  9(03)       COMP-3.
40  03     05  TX-AG-CONTAB               PIC  9(05)       COMP-3.
43  01     05  TX-AP                      PIC  9(01).
44  02     05  TX-MES-BASE                PIC  9(02).
46  04     05  TX-ANO-BASE                PIC  9(04).
50  05     05  TX-NUMERO-CLIENTE          PIC  9(09)       COMP-3.
55  03     05  TX-FILIAL-CLIENTE          PIC  9(04)       COMP-3.
58  02     05  TX-CONTR-CLIENTE           PIC  9(02)       COMP-3.
60  01     05  TX-TIPO-CLIENTE            PIC  9(01).
61  40     05  TX-NOME-CLIENTE            PIC  X(40).
101 02     05  TX-PRAZO                   PIC  9(02).
103 09     05  TX-VALOR                   PIC  9(15)V9(02) COMP-3.
112 03     05  TX-TIPO-GARANTIA           PIC  9(04)       COMP-3.
115 05     05  TX-NUMERO-COOBRIGADO       PIC  9(09)       COMP-3.
120 03     05  TX-FILIAL-COOBRIGADO       PIC  9(04)       COMP-3.
123 02     05  TX-CONTR-COOBRIGADO        PIC  9(02)       COMP-3.
125 02     05  TX-CLASS-OPERACAO          PIC  X(02).
127 02     05  TX-NATUREZA-OPERACAO       PIC  9(02).
129 04     05  TX-ORIGEM-RECURSOS         PIC  9(04).
133 02     05  TX-TAXA-REFERENCIAL        PIC  9(02).
135 03     05  TX-VARIACAO-CAMBIAL        PIC  9(03).
138 06     05  TX-TAXA-EFETIVA-ANUAL      PIC  9(04)V9(07) COMP-3.
144 02     05  TX-CARAC-ESPECIAL          PIC  9(02).
146 05     05  TX-DT-INICIO-OPERACAO      PIC  9(08)       COMP-3.
151 05     05  TX-DT-MENOR-VENCIMENTO     PIC  9(08)       COMP-3.
156 05     05  TX-DT-MAIOR-VENCIMENTO     PIC  9(08)       COMP-3.
161 09     05  TX-CONTRATO-LCOMPROMI      PIC  X(17).
170 10     05  TX-RESERVA                 PIC  X(03).
