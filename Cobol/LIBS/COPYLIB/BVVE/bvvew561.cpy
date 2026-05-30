      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW561                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3561             *
      * DATA      : 06/09/2020                                         *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00293 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW561-REGISTRO.
              10 BVVEW561-BLOCO-ENTRADA.
                 15 BVVEW561-E-DATA-CONSULTA      PIC  X(010).
                 15 BVVEW561-E-CMNTR-MSGRA        PIC  X(023).
              10 BVVEW561-BLOCO-SAIDA.
                 15 BVVEW561-S-CUNIC-TRFGO-MSGEM  PIC  X(023).
                 15 BVVEW561-S-EVENTO.
                    20 BVVEW561-S-CSIST-EXTER     PIC  X(003).
                    20 BVVEW561-S-CEVNTO-MSGEM    PIC  X(004).
                    20 BVVEW561-S-CFLUXO-MSGEM    PIC  X(002).
                 15 BVVEW561-S-CISPB-EMISR-MSGEM  PIC  X(008).
                 15 BVVEW561-S-CISPB-DSTNA-MSGEM  PIC  X(008).
                 15 BVVEW561-S-HPROCM-MSGEM       PIC  X(026).
                 15 BVVEW561-S-HCONF-ENVIO-MSGEM  PIC  X(026).
                 15 BVVEW561-S-HCONF-LEITU-MSGEM  PIC  X(026).
                 15 BVVEW561-S-RIDTFD-MSGEM       PIC  X(048).
                 15 BVVEW561-S-CDOMNO-MSGRA-SIST  PIC  X(005).
                 15 BVVEW561-S-CFASE-PROCM        PIC  9(004).
                 15 BVVEW561-S-RFASE-PROCM        PIC  X(100).
                 15 BVVEW561-S-HINIC-ETAPA-MNTRC  PIC  X(026).
                 15 BVVEW561-S-HFNAL-ETAPA-MNTRC  PIC  X(026).
                 15 BVVEW561-S-CINDCD-RPROC-MSGEM PIC  9(001).
                 15 BVVEW561-S-CSNLZD-MSGRA       PIC  9(003).
MB1407           15 BVVEW561-S-CCTRL-RQUIS-INSTC  PIC  X(020).
MB0208           15 BVVEW561-S-COPER-ENVID-CIPAG  PIC  X(020).
              10 BVVEW561-BLOCO-RETORNO.
                 15 BVVEW561-COD-RET              PIC  9(002).
                 15 BVVEW561-MSG-RET              PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
