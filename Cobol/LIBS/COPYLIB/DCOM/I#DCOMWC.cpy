      *---------------------------------------------------------------*
      *   NOME INC             -DDNAME-                LENGTH  0100   *
      *   I#DCOMWC              ------                 SISTEMA DCOM   *
      *---------------------------------------------------------------*
      *                                                               *
      *   DESCRICAO: LAYOUT INTERFACE PARA O SISTEMA CEPT             *
      *                                                               *
      *   PROPOSTA DDS - 04/0295.                                     *
      *---------------------------------------------------------------*
      *
       01  DCOMWC-REG.
           05  DCOMWC-AGENCIA          PIC 9(05) COMP-3.
           05  DCOMWC-CONTA            PIC 9(13) COMP-3.
           05  DCOMWC-RAZAO.
               10  DCOMWC-GRUPO        PIC 9(03) COMP-3.
               10  DCOMWC-SUBGRUPO     PIC 9(03) COMP-3.
           05  DCOMWC-CNPJCPF.
               10  DCOMWC-PRINCIPAL    PIC 9(09) COMP-3.
               10  DCOMWC-FILIAL       PIC 9(05) COMP-3.
               10  DCOMWC-CONTROLE     PIC 9(02) COMP-3.
           05  DCOMWC-MESANOREF        PIC 9(06).
      ******************************************************
      * DESCRICAO DO CAMPO PRODUTO                         *
      ******************************************************
      * 01 - CONTA GARANTIA                                *
      * 02 - EMPRESTIMOS                                   *
      * 03 - HOT MONEY                                     *
      * 04 - DUPLICATAS DESCONTADAS                        *
      * 05 - CHEQUES DESCONTADOS                           *
      * 06 - NP DESCONTADAS                                *
      ******************************************************
           05  DCOMWC-PRODUTO          PIC 9(02).
           05  DCOMWC-VL-CTA-GARANTIA  PIC 9(15)V99 COMP-3.
           05  DCOMWC-VL-EMPRESTIMO    PIC 9(15)V99 COMP-3.
           05  DCOMWC-VL-HOT-MONEY     PIC 9(15)V99 COMP-3.
           05  DCOMWC-VL-DUPLICATAS    PIC 9(15)V99 COMP-3.
           05  DCOMWC-VL-CHEQUES       PIC 9(15)V99 COMP-3.
           05  DCOMWC-VL-NOTAS-PROMIS  PIC 9(15)V99 COMP-3.
           05  DCOMWC-CCUSTO           PIC X(04).
           05  FILLER                  PIC X(10).
