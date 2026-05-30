      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW564                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3564             *
      * DATA      : 14/09/2017                                         *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00504 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW564-REGISTRO.
              10 BVVEW564-BLOCO-ENTRADA.
                 15 BVVEW564-E-DATA-CONSULTA      PIC  X(010).
                 15 BVVEW564-E-CMNTR-MSGRA        PIC  X(023).
                 15 BVVEW564-E-NETAPA-MNTRC-MSGRA PIC  9(002).
                 15 BVVEW564-E-CSIST-EXTER        PIC  X(003).
              10 BVVEW564-BLOCO-SAIDA.
                 15 BVVEW564-S-CFASE-PROCM        PIC  9(004).
                 15 BVVEW564-S-RFASE-PROCM        PIC  X(100).
                 15 BVVEW564-S-HINIC-ETAPA-MNTRC  PIC  X(026).
                 15 BVVEW564-S-RERRO-ETAPA-MNTRC  PIC  X(255).
              10 BVVEW564-BLOCO-RETORNO.
                 15 BVVEW564-COD-RET              PIC  9(002).
                 15 BVVEW564-MSG-RET              PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
