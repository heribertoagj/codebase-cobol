      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW563                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3562             *
      * DATA      : 09/09/2020                                         *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 01931 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      *                                                                *
      * BVVEW563-E-SENTIDO-PAGINACAO:  'A' - ANTERIOR                  *
      *                                'I' - INICIAL                   *
      *                                'P' - PROXIMO                   *
      * BVVEW563-E-CUNIC-TRFGO-MSGEM:  - OPCIONAL                      *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW563-REGISTRO.
              10 BVVEW563-BLOCO-ENTRADA.
                 15 BVVEW563-E-SENTIDO-PAGINACAO     PIC  X(001).
                 15 BVVEW563-E-DATA-CONSULTA         PIC  X(010).
                 15 BVVEW563-E-CMNTR-MSGRA           PIC  X(023).
                 15 BVVEW563-E-CUNIC-TRFGO-MSGEM     PIC  X(023).
              10 BVVEW563-BLOCO-RESTART.
                 15 BVVEW563-R-HINIC-ETAPA-INI       PIC  X(026).
                 15 BVVEW563-R-HFNAL-ETAPA-INI       PIC  X(026).
                 15 BVVEW563-R-HINIC-ETAPA-FIM       PIC  X(026).
                 15 BVVEW563-R-HFNAL-ETAPA-FIM       PIC  X(026).
              10 BVVEW563-BLOCO-SAIDA.
                 15 BVVEW563-S-QTDE-CONSULTA         PIC  9(009).
                 15 BVVEW563-S-MAIS-REGISTROS        PIC  X(001).
                 15 BVVEW563-S-CUNIC-TRFGO-MSGEM     PIC  X(023).
                 15 BVVEW563-S-EVENTO.
                    20 BVVEW563-S-CSIST-EXTER        PIC  X(003).
                    20 BVVEW563-S-CEVNTO-MSGEM       PIC  X(004).
                    20 BVVEW563-S-CFLUXO-MSGEM       PIC  X(002).
                 15 BVVEW563-S-CISPB-EMISR-MSGEM     PIC  X(008).
                 15 BVVEW563-S-CISPB-DSTNA-MSGEM     PIC  X(008).
                 15 BVVEW563-S-HPROCM-MSGEM          PIC  X(026).
                 15 BVVEW563-S-HCONF-ENVIO-MSGEM     PIC  X(026).
                 15 BVVEW563-S-HCONF-LEITU-MSGEM     PIC  X(026).
                 15 BVVEW563-S-RIDTFD-MSGEM          PIC  X(048).
                 15 BVVEW563-S-CDOMNO-MSGRA-SIST     PIC  X(005).
                 15 BVVEW563-S-DADOS-CONSULTA        OCCURS 08 TIMES.
                    20 BVVEW563-S-CMNTRC-MSGRA       PIC  X(023).
                    20 BVVEW563-S-NETAPA-MNTRC-MSGRA PIC  9(002).
                    20 BVVEW563-S-HINIC-ETAPA-MNTRC  PIC  X(026).
                    20 BVVEW563-S-HFNAL-ETAPA-MNTRC  PIC  X(026).
                    20 BVVEW563-S-CFASE-PROCM        PIC  9(004).
                    20 BVVEW563-S-RFASE-PROCM        PIC  X(100).
              10 BVVEW563-BLOCO-RETORNO.
                 15 BVVEW563-COD-RET                 PIC  9(002).
                 15 BVVEW563-MSG-RET                 PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
