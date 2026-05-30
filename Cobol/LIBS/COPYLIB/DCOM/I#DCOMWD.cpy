      *---------------------------------------------------------------*
      *   NOME INC             DDNAME                  LENGTH  0307   *
      *   I#DCOMWD             SAICOMPL                SISTEMA DCOM   *
      *---------------------------------------------------------------*
      *   BASE DAS INTERFACES:                                        *
      *            EMPI - PAGOS NO VENCIMENTO                         *
      *            EMPI - ESTOQUE                                     *
      *---------------------------------------------------------------*
       01  DCOMWD-REG.
           05  DCOMWD-CBCO             PIC 9(03)       COMP-3.
           05  DCOMWD-AGENCIA          PIC 9(05)       COMP-3.
           05  DCOMWD-RAZAO.
               10  DCOMWD-GRUPO        PIC 9(03)       COMP-3.
               10  DCOMWD-SUBGRUPO     PIC 9(03)       COMP-3.
           05  DCOMWD-CONTA            PIC 9(13)       COMP-3.
           05  DCOMWD-PRODUTO          PIC 9(03)       COMP-3.
           05  DCOMWD-SUBPROD          PIC 9(03)       COMP-3.
           05  DCOMWD-NUM-OPERACAO.
               10  DCOMWD-ANO-OPE      PIC 9(04)       COMP-3.
               10  DCOMWD-SEQ-OPE      PIC 9(09)       COMP-3.
           05  DCOMWD-NUM-PARCELA      PIC 9(05)       COMP-3.
           05  DCOMWD-SIT-PARCELA      PIC 9(03)       COMP-3.
           05  DCOMWD-TIPO-DESCONTO    PIC 9(03)       COMP-3.
           05  DCOMWD-QTDE-CHEQUES     PIC 9(09)       COMP-3.
           05  DCOMWD-DATA-DESCONTO    PIC X(10).
           05  DCOMWD-VCTO-PARCELA     PIC X(10).
           05  DCOMWD-PRAZO-EM-DIAS    PIC 9(04)       COMP-3.
           05  DCOMWD-CARTEIRA         PIC X(05).
      *********************************************************
      *  CARTEIRA A VENCER = WA-C9-CSIT-DESC-COML (24 OU 37)  *
      *  CARTEIRA VENCIDA  = WA-C9-CSIT-DESC-COML             *
      *                     (06 07 19 20)                     *
      *********************************************************
           05  DCOMWD-MEIO-ENTRADA     PIC 9(03)       COMP-3.
           05  DCOMWD-PTX-JURO-MES     PIC 9(03)V9(05) COMP-3.
           05  DCOMWD-TX-ESPECIAL      PIC 9(01).
      ******************************************************
      *            DADOS DO CEDENTE                        *
      ******************************************************
           05  DCOMWD-DADOS-CEDENTE.
               10  DCOMWD-CPF-CNPJ.
                   15  DCOMWD-NUM-CD   PIC 9(09)       COMP-3.
                   15  DCOMWD-FIL-CD   PIC 9(05)       COMP-3.
                   15  DCOMWD-CTR-CD   PIC 9(02)       COMP-3.
               10  DCOMWD-NOME-CD      PIC X(60).
               10  DCOMWD-TPPS-CD      PIC X(01).
      ******************************************************
      *            DADOS DO SACADO                         *
      ******************************************************
           05  DCOMWD-DADOS-SACADO.
               10  DCOMWD-CPF-CNPJ-SA.
                   15  DCOMWD-NUM-SA   PIC 9(09)       COMP-3.
                   15  DCOMWD-FIL-SA   PIC 9(05)       COMP-3.
                   15  DCOMWD-CTR-SA   PIC 9(02)       COMP-3.
               10  DCOMWD-NOME-SA      PIC X(60).
               10  DCOMWD-ENDE-SA      PIC X(40).
               10  DCOMWD-CCEP-SA      PIC 9(05)       COMP-3.
               10  DCOMWD-SCEP-SA      PIC 9(03)       COMP-3.
      ******************************************************
      *            DADOS FINANCEIROS                       *
      ******************************************************
           05  DCOMWD-VL-PRINCIPAL     PIC 9(15)V99    COMP-3.
           05  DCOMWD-VL-PRESENTE      PIC 9(15)V99    COMP-3.
           05  DCOMWD-SALDO-DV-PRINC   PIC 9(15)V99    COMP-3.
           05  DCOMWD-VL-IOF-VENCIDO   PIC 9(15)V99    COMP-3.
           05  DCOMWD-VL-JURO-MORA-GR  PIC 9(15)V99    COMP-3.
