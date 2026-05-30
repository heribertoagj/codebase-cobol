      *---------------------------------------------------------------*
      *   NOME INC             -DDNAME-                LENGTH  0051   *
      *   I#DCOMGI              ------                 SISTEMA DCOM   *
      *---------------------------------------------------------------*
      *                                                               *
      *   DESCRICAO: LAYOUT - DCOM FORNECERA AO SISTEMA GIPS UM ARQ.  *
      *                                                               *
      *   PROPOSTA DDS - 09/0208.                                     *
      *---------------------------------------------------------------*

       01  DCOMGI-REG.
 1  2      05 GI-BANCO                       PIC  9(03)        COMP-3.
 3  3      05 GI-AGENCIA                     PIC  9(05)        COMP-3.
 6  7      05 GI-CONTA                       PIC  9(13)        COMP-3.
13  2      05 GI-PRODUTO                     PIC  9(03)        COMP-3.
15  2      05 GI-SUBPRODUTO                  PIC  9(03)        COMP-3.
17  3      05 GI-CARTEIRA-GIPS               PIC  X(03).
20  4      05 GI-MES-ANO-REFERENC            PIC  9(06)        COMP-3.
24  9      05 GI-VALOR-ENTRADA-MES           PIC  9(15)V9(02)  COMP-3.
33  9      05 GI-QUANT-NOVAS-OPER            PIC  9(09)        COMP-3.
42  9      05 GI-VALOR-BAIXA-MES             PIC  9(15)V9(02)  COMP-3.
51  5      05 GI-QUANT-OPER-BAIXADAS         PIC  9(09)        COMP-3.
