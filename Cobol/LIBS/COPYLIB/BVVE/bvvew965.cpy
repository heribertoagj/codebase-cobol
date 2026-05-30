      ******************************************************************
      * INC         : BVVEW965                                         *
      * DESCRICAO   : AREA DE COMUNICACAO DO PROGRAMA TMNF8965         *
      * TAMANHO     : 31070 BYTES                                      *
      * DATA CRIACAO: 17/04/2014                                       *
      * AUTOR       : 7COMM                                            *
      ******************************************************************
           05 BVVEW965-REGISTRO.
              10 BVVEW965-CTRANS-PROG-APRES        PIC  X(004).
              10 BVVEW965-CTERM-USUAR              PIC  X(015).
              10 BVVEW965-CMNTRC-MSGRA             PIC  X(023).
              10 BVVEW965-E-OPCAO-PESQ             PIC  9(001).
              10 BVVEW965-FLUXO-MSGEM              PIC  X(001).
              10 BVVEW965-TAMANHO-SISMSG           PIC  9(005).
              10 BVVEW965-SISMSG-REMODELADA        PIC  X(31000).
              10 FILLER                            PIC  X(021).
