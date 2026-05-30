      *********************************************************
      * SISTEMA   : DCOM  DESCONTO COMERCIAL   NOME: I#DCOMUO *
      * DESCRICAO : TABELA DE SOLICITACAO DE ENCERRAMENTO     *
      *             + RETORNO DO COSA                         *
      *                                                       *
      * TAMANHO   : LRECL - 0170                              *
      *                                                       *
      *********************************************************
       01 UO-REGISTRO.
          05 UO-REGISTRO-DCOMB0P3.
             10 UO-NSOLTC-ENCRR-CTA     PIC S9(17)V USAGE COMP-3.
             10 UO-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
             10 UO-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
             10 UO-CBCO                 PIC S9(3)V USAGE COMP-3.
             10 UO-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
             10 UO-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
             10 UO-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
             10 UO-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
             10 UO-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
             10 UO-CSIT-SOLTC-ENCRR     PIC X(1).
             10 UO-DSOLTC-ENCRR-CTA     PIC X(10).
             10 UO-DPREVT-EFTVC-DSVNC   PIC X(10).
             10 UO-CPSSOA-JURID-CTA     PIC S9(10)V USAGE COMP-3.
             10 UO-CTPO-NEGOC-CTA       PIC S9(3)V USAGE COMP-3.
             10 UO-NSEQ-NEGOC-CTA       PIC S9(10)V USAGE COMP-3.
             10 UO-CTPO-SOLTC-ENCRR     PIC S9(1)V USAGE COMP-3.
             10 UO-CMOTVO-ENCRR-CTA     PIC S9(3)V USAGE COMP-3.
             10 UO-CFORMA-DSVNC-CTA     PIC S9(3)V USAGE COMP-3.
             10 UO-CBCO-SOLCT-ENCRR     PIC S9(3)V USAGE COMP-3.
             10 UO-CAG-SOLCT-ENCRR      PIC S9(5)V USAGE COMP-3.
             10 UO-CUSUAR-RESP          PIC X(9).
             10 UO-HATULZ               PIC X(26).
             10 UO-CTRANS-PROG-ATULZ    PIC X(8).
          05 UO-REGISTRO-COMPLEM.
             10 UO-SIT-CONTA-COSA       PIC  9(01).
             10 UO-HORA-COSA            PIC  9(06).
             10 UO-COD-RETORNO-COSA     PIC  9(07)     COMP-3.
             10 UO-VLR-TOTAL-OPER       PIC S9(13)V99  COMP-3.
             10 UO-SALDANT-LIVRE-COSA   PIC S9(13)V99  COMP-3.
             10 UO-SALDANT-RESDISP-COSA PIC S9(13)V99  COMP-3.
             10 UO-SALDANT-TOTAL-COSA   PIC S9(13)V99  COMP-3.
             10 UO-DCONSULTA-ENCER      PIC X(10).
             10 UO-SOLICITACAO-BAIXA    PIC X(01).
             10 FILLER                  PIC X(19).
