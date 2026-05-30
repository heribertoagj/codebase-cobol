      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW560                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO PROGRAMA BVVE3560           *
      * DATA      : 04/09/2020                                         *
      * AUTOR     : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 02645 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW560-E-CHAMADOR        :   PROGRAMA CHAMADOR               *
      * BVVEW560-E-OPCAO-PESQ      :   INDICA QUAL ACESSO DB2 FAZER    *
      * BVVEW560-E-DATA-CONSULTA   :   DIA DESEJADO DA CONSULTA        *
      * BVVEW560-E-HORA-CONSULTA   :   HORA DESEJADA DA CONSULTA       *
      * BVVEW560-E-SENTIDO-PAGINACAO:  'I' - INICIAL                   *
      *                                'A' - ANTERIOR                  *
      *                                'P' - PROXIMO                   *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW560-REGISTRO.
              10 BVVEW560-BLOCO-ENTRADA.
                 15 BVVEW560-E-SENTIDO-PAGINACAO      PIC  X(001).
                 15 BVVEW560-E-DADOS-CONSULTA.
                    20 BVVEW560-E-DATA-CONSULTA       PIC  X(010).
                    20 BVVEW560-E-HORA-CONSULTA-INI   PIC  X(008).
                    20 BVVEW560-E-HORA-CONSULTA-FIM   PIC  X(008).
                    20 BVVEW560-E-CSNLZD-MSGRA        PIC  9(003).
                    20 BVVEW560-E-CISPB-DSTNA-MSGEM   PIC  X(008).
                    20 BVVEW560-E-CISPB-EMISR-MSGEM   PIC  X(008).
                    20 BVVEW560-E-CCTRL-RQUIS-INSTC   PIC  X(020).
                    20 BVVEW560-E-CUNIC-TRFGO-MSGEM   PIC  X(023).
MB0208              20 BVVEW560-E-CCONTR              PIC  X(020).
                    20 BVVEW560-E-EVENTO.
                       25 BVVEW560-E-CSIST-EXTER      PIC  X(003).
                       25 BVVEW560-E-CEVNTO-MSGEM     PIC  X(004).
                       25 BVVEW560-E-CFLUXO-MSGEM     PIC  X(002).
                    20 BVVEW560-E-FLUXO-MSGEM         PIC  X(001).
              10 BVVEW560-BLOCO-RESTART.
                 15 BVVEW560-R-CMNTRC-MSGRA-INI       PIC  X(023).
                 15 BVVEW560-R-CMNTRC-MSGRA-FIM       PIC  X(023).
              10 BVVEW560-BLOCO-SAIDA.
                 15 BVVEW560-S-QTDE-CONSULTA          PIC  9(009).
                 15 BVVEW560-S-MAIS-REGISTROS         PIC  X(001).
                 15 BVVEW560-S-DADOS-CONSULTA OCCURS 12 TIMES.
                    20 BVVEW560-S-CMNTRC-MSGRA        PIC  X(023).
                    20 BVVEW560-S-CSNLZD-MSGRA        PIC  9(003).
                    20 BVVEW560-S-RSNLZD-MSGRA        PIC  X(100).
                    20 BVVEW560-S-HPROCM-MSGEM        PIC  X(026).
                    20 BVVEW560-S-CISPB-DSTNA-MSGEM   PIC  X(008).
                    20 BVVEW560-S-CISPB-EMISR-MSGEM   PIC  X(008).
                    20 BVVEW560-S-CUNIC-TRFGO-MSGEM   PIC  X(023).
                    20 BVVEW560-S-EVENTO.
                       25 BVVEW560-S-CSIST-EXTER      PIC  X(003).
                       25 BVVEW560-S-CEVNTO-MSGEM     PIC  X(004).
                       25 BVVEW560-S-CFLUXO-MSGEM     PIC  X(002).
               10 BVVEW560-BLOCO-RETORNO.
                   15 BVVEW560-COD-RET                PIC  9(002).
                   15 BVVEW560-MSG-RET                PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
