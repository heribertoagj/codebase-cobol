      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEW590                                           *
      * DESCRICAO : BOOK DE COMUNICACAO DO MODULO BVVE3590             *
      * DATA      : 13/09/2017                                         *
      * EMPRESA   : 7COMM INFORMATICA                                  *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 132 BYTES                                          *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 BVVEW590-REGISTRO.
              10 BVVEW590-BLOCO-ENTRADA.
                 15 BVVEW590-E-CMNTRC-MSGRA       PIC  X(023).
                 15 BVVEW590-E-CISPB-PARTC        PIC  X(008).
                 15 BVVEW590-E-DATA-CONSULTA      PIC  X(010).
                 15 BVVEW590-E-CUSUAR-MANUT       PIC  X(009).
                 15 BVVEW590-E-EVENTO             PIC  X(009).
              10 BVVEW590-BLOCO-SAIDA.
                 15 BVVEW590-S-AUTORIZADO         PIC  X(001).
              10 BVVEW590-BLOCO-RETORNO.
                 15 BVVEW590-COD-RET              PIC  9(002).
                 15 BVVEW590-MSG-RET              PIC  X(079).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      ******************************************************************
