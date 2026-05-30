      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW562                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3562             *
      * DATA      : 08/09/2020                                         *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 02963 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW562-E-SENTIDO-PAGINACAO:  'A' - ANTERIOR                  *
      *                                'I' - INICIAL                   *
      *                                'P' - PROXIMO                   *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW562-REGISTRO.
              10 BVVEW562-BLOCO-ENTRADA.
                 15 BVVEW562-E-SENTIDO-PAGINACAO     PIC  X(001).
                 15 BVVEW562-E-DATA-CONSULTA         PIC  X(010).
                 15 BVVEW562-E-CMNTR-MSGRA           PIC  X(023).
MB1907           15 BVVEW562-E-CCTRL-RQUIS-INSTC     PIC  X(020).
              10 BVVEW562-BLOCO-RESTART.
                 15 BVVEW562-R-CMNTRC-MSGRA-INI      PIC  X(023).
                 15 BVVEW562-R-CMNTRC-MSGRA-FIM      PIC  X(023).
              10 BVVEW562-BLOCO-SAIDA.
                 15 BVVEW562-S-QTDE-CONSULTA         PIC  9(009).
                 15 BVVEW562-S-MAIS-REGISTROS        PIC  X(001).
                 15 BVVEW562-S-DADOS-CONSULTA        OCCURS 12 TIMES.
                    20 BVVEW562-S-CMNTRC-MSGRA       PIC  X(023).
                    20 BVVEW562-S-EVENTO.
                       25 BVVEW562-S-CSIST-EXTER     PIC  X(003).
                       25 BVVEW562-S-CEVNTO-MSGEM    PIC  X(004).
                       25 BVVEW562-S-CFLUXO-MSGEM    PIC  X(002).
                    20 BVVEW562-S-CISPB-EMISR-MSGEM  PIC  X(008).
                    20 BVVEW562-S-CISPB-DSTNA-MSGEM  PIC  X(008).
                    20 BVVEW562-S-CSNLZD-MSGRA       PIC  9(003).
                    20 BVVEW562-S-RSNLZD-MSGRA       PIC  X(100).
                    20 BVVEW562-S-HPROCM-MSGEM       PIC  X(026).
                    20 BVVEW562-S-RIDTFD-MSGEM       PIC  X(048).
                    20 BVVEW562-S-CDOMNO-MSGRA-SIST  PIC  X(005).
              10 BVVEW562-BLOCO-RETORNO.
                 15 BVVEW562-COD-RET                 PIC  9(002).
                 15 BVVEW562-MSG-RET                 PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
