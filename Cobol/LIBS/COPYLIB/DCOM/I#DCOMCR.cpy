      ******************************************************************
      * SISTEMA : DCOM X RISC  -  I#DCOMCR                             *
BRQ001* TAMANHO : 0400 BYTES                                           *
      * ARQUIVO : DADOS DO RISCO DE CREDITO                            *
      ******************************************************************
BRQ001*                   B R Q  -  A L T E R A C A O                  *
BRQ001*----------------------------------------------------------------*
BRQ001*    ANALISTA BRQ:  FERNANDO LUIZ DE SANTI                       *
BRQ001*    ANALISTA DS.:  RICARDO JAMMAL          -  GRUPO 39          *
BRQ001*    DATA........:  19/03/2019                                   *
BRQ001*----------------------------------------------------------------*
BRQ001*    OBJETIVO....:  BRQ 19/0076                                  *
BRQ001*      ALTERAR O LAYOUT DO ARQUIVO DE CARACTERISTICA ESPECIAL.   *
BRQ001******************************************************************
      *
BRQ001 01  CR-REGISTRO.
BRQ001   03  CR-HEADER.
01  02     05  CR-HEA-TP-REG             PIC 9(002).
03  04     05  CR-HEA-ROTINA             PIC X(004).
07  10     05  CR-HEA-DATA-BASE          PIC X(010).
17  10     05  CR-HEA-DATA-GERACAO       PIC X(010).
27  01     05  CR-HEA-TP-MOVTO           PIC X(001).
28 373     05  CR-HEA-RESERVA            PIC X(373).
      *
BRQ001   03  CR-DETALHE                  REDEFINES CR-HEADER.
01  02     05  CR-TP-REGISTRO            PIC  9(002).
03  01     05  CR-MOTVO-SUBSTITUICAO     PIC  X(001).
04  05     05  CR-NR-CNPJ-CPF            PIC  9(009)   COMP-3.
09  03     05  CR-FLIAL-CNPJ-CPF         PIC  9(004)   COMP-3.
12  02     05  CR-CTRL-CNPJ-CPF          PIC  9(002)   COMP-3.
14  01     05  CR-TP-CLIENTE             PIC  9(001)   COMP-3.
15  04     05  CR-CENTRO-CUSTO           PIC  X(004).
19  03     05  CR-AGENCIA                PIC  9(005)   COMP-3.
22  04     05  CR-CONTA                  PIC  9(007)   COMP-3.
26  03     05  CR-CARTEIRA               PIC  X(003).
29  09     05  CR-CONTRATO               PIC  9(017)   COMP-3.
38  03     05  CR-EMPRESA                PIC  9(005)   COMP-3.
41  02     05  CR-CARAC-ESPCL-01         PIC  9(003)   COMP-3.
43  02     05  CR-CARAC-ESPCL-02         PIC  9(003)   COMP-3.
45  02     05  CR-CARAC-ESPCL-03         PIC  9(003)   COMP-3.
47  02     05  CR-CARAC-ESPCL-04         PIC  9(003)   COMP-3.
49  02     05  CR-CARAC-ESPCL-05         PIC  9(003)   COMP-3.
51  02     05  CR-CARAC-ESPCL-06         PIC  9(003)   COMP-3.
53  02     05  CR-CARAC-ESPCL-07         PIC  9(003)   COMP-3.
55  02     05  CR-CARAC-ESPCL-08         PIC  9(003)   COMP-3.
57  02     05  CR-CARAC-ESPCL-09         PIC  9(003)   COMP-3.
59  02     05  CR-CARAC-ESPCL-10         PIC  9(003)   COMP-3.
61 340     05  CR-RESERVA                PIC  X(340).
