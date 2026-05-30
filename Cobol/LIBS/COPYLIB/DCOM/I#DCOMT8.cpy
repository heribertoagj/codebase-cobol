      ******************************************************************
      * SISTEMA : DCOM - I#DCOMT5                                      *
      * TAMANHO : 4145 BYTES                                           *
      * ARQUIVO : TABELA DE MENSAGENS DO RECOR                         *
      * INCLUDE DA TABELA DCOMB0L7 - DB2PRD.TMNTRC_MSGEM_DESC          *
      ******************************************************************

       01  T8-L7-REGISTRO.
           05 T8-L7-DANO-OPER-DESC     PIC S9(4)V USAGE COMP-3.
           05 T8-L7-NSEQ-OPER-DESC     PIC S9(9)V USAGE COMP-3.
           05 T8-L7-HINCL-REG          PIC X(26).
           05 T8-L7-CMSGEM-RECOR       PIC X(9).
           05 T8-L7-CISPB-EMISR-MSGEM  PIC X(8).
           05 T8-L7-CISPB-DSTNA-MSGEM  PIC X(8).
           05 T8-L7-CCTRL-RQUIS-FINCR  PIC X(20).
           05 T8-L7-CUNIC-MSGEM        PIC X(23).
           05 T8-L7-CSIT-DESC-COML     PIC S9(3)V USAGE COMP-3.
           05 T8-L7-HULT-ATULZ         PIC X(26).
           05 T8-L7-WCONTD-MSGEM.
              10 T8-L7-WCONTD-MSGEM-LEN
                                       PIC S9(4) USAGE COMP.
              10 T8-L7-WCONTD-MSGEM-TEXT
                                       PIC X(4000).
           05 T8-L7-CFUNC-BDSCO        PIC S9(9)V USAGE COMP-3.
           05 T8-L7-CTERM              PIC X(8).
