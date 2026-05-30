      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW583                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3583             *
      * DATA      : 06/09/2020                                         *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 02963 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW583-E-SENTIDO-PAGINACAO:  'A' - ANTERIOR                  *
      *                                'I' - INICIAL                   *
      *                                'P' - PROXIMO                   *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW583-REGISTRO.
              10 BVVEW583-BLOCO-ENTRADA.
                 15 BVVEW583-E-SENTIDO-PAGINACAO     PIC  X(001).
                 15 BVVEW583-E-DATA-CONSULTA         PIC  X(010).
                 15 BVVEW583-E-CMNTR-MSGRA           PIC  X(023).
                 15 BVVEW583-E-RIDTFD-MSGEM          PIC  X(048).
                 15 BVVEW583-E-CINDCD-ORIGE          PIC  9(001).
              10 BVVEW583-BLOCO-RESTART.
                 15 BVVEW583-R-CMNTRC-MSGRA-INI      PIC  X(023).
                 15 BVVEW583-R-CMNTRC-MSGRA-FIM      PIC  X(023).
              10 BVVEW583-BLOCO-SAIDA.
                 15 BVVEW583-S-QTDE-CONSULTA         PIC  9(009).
                 15 BVVEW583-S-MAIS-REGISTROS        PIC  X(001).
                 15 BVVEW583-S-DADOS-CONSULTA        OCCURS 12 TIMES.
                    20 BVVEW583-S-CMNTRC-MSGRA       PIC  X(023).
                    20 BVVEW583-S-EVENTO.
                       25 BVVEW583-S-CSIST-EXTER     PIC  X(003).
                       25 BVVEW583-S-CEVNTO-MSGEM    PIC  X(004).
                       25 BVVEW583-S-CFLUXO-MSGEM    PIC  X(002).
                    20 BVVEW583-S-CISPB-EMISR-MSGEM  PIC  X(008).
                    20 BVVEW583-S-CISPB-DSTNA-MSGEM  PIC  X(008).
                    20 BVVEW583-S-CSNLZD-MSGRA       PIC  9(003).
                    20 BVVEW583-S-RSNLZD-MSGRA       PIC  X(100).
                    20 BVVEW583-S-HPROCM-MSGEM       PIC  X(026).
                    20 BVVEW583-S-CUNIC-TRFGO        PIC  X(023).
                    20 BVVEW583-S-CDOMNO-MSGRA-SIST  PIC  X(005).
              10 BVVEW583-BLOCO-RETORNO.
                 15 BVVEW583-COD-RET                 PIC  9(002).
                 15 BVVEW583-MSG-RET                 PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
