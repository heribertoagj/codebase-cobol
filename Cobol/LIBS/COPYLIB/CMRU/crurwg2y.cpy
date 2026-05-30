      ******************************************************************
      * NOME BOOK : CRURWG2Y                                           *
      * DESCRICAO : BOOK INTERFACE - CRUR2G2C                          *
      * DATA      : 23/05/2024                                         *
      * AUTOR     : ANDERSON PANARIELLO                                *
      * EMPRESA   : STEFANINI                                          *
      * GRUPO     : STEFANINI                                          *
      * COMPONENTE:                                                    *
      * TAMANHO   : 0290 BYTES                                         *
      ******************************************************************
      * AREA DE ENTRADA:                                               *
      * ----------------                                               *
      * CCLIMT-RURAL         :                                         *
      ******************************************************************
      * AREA DE SAIDA:                                                 *
      * ----------------                                               *
      * COUTRO-TRATO         :                                         *
      * COUTRO-SOLO          :                                         *
      * ROUTRO-SOLO          :                                         *
      * DINIC-VGCIA-REG      : DATA DE INICIO DE VIGENCIA              *
      * DFIM-VGCIA-REG       : DATA DE FIM DE VIGENCIA                 *
      * CSIT-REG-RURAL       : SITUACAO DO REGISTRO DO RURAL           *
      * CUSUAR-INCL          : USUARIO DE INCLUSAO REGISTRO            *
      * HINCL-REG            : DATA E HORA INCLUSAO REGISTRO           *
      * CUSUAR-MANUT         : USUARIO DE MANUTENCAO DO REGISTRO       *
      * HMANUT-REG           : DATA E HORA DE MANUTENCAO DO REGISTRO   *
      ******************************************************************
       05 CRURWG2Y-HEADER.
           10 CRURWG2Y-COD-LAYOUT       PIC X(008)  VALUE 'CRURWG2Y'.
           10 CRURWG2Y-TAM-LAYOUT       PIC 9(005)  VALUE 00296.
       05 CRURWG2Y-REGISTRO.
           10 CRURWG2Y-BLOCO-ENTRADA.
             15 CRURWG2Y-E-COUTRO-SOLO            PIC 9(02).
           10 CRURWG2Y-S-BLOCO-SAIDA.
             15 CRURWG2Y-S-COUTRO-TRATO           PIC 9(05).
             15 CRURWG2Y-S-COUTRO-SOLO            PIC 9(02).
             15 CRURWG2Y-S-ROUTRO-SOLO            PIC X(80).
             15 CRURWG2Y-S-DINIC-VGCIA-REG        PIC X(10).
             15 CRURWG2Y-S-DFIM-VGCIA-REG         PIC X(10).
             15 CRURWG2Y-S-CSIT-REG-RURAL         PIC X(01).
             15 CRURWG2Y-S-CUSUAR-INCL            PIC X(09).
             15 CRURWG2Y-S-HINCL-REG              PIC X(26).
             15 CRURWG2Y-S-CUSUAR-MANUT           PIC X(09).
             15 CRURWG2Y-S-HMANUT-REG             PIC X(26).
             15 CRURWG2Y-S-NOME-USUARIO           PIC X(40).
             15 CRURWG2Y-S-NOME-DEPTO             PIC X(60).
