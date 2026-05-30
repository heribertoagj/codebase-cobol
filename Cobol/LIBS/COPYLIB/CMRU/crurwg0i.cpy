      ******************************************************************
      * NOME BOOK : CRURWG0I                                           *
      * DESCRICAO : BOOK INTERFACE - CRUR3G0C                          *
      * DATA      : 13/06/2012                                         *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : PRIME IT                                           *
      * GRUPO     : PRIME IT                                           *
      * COMPONENTE:                                                    *
      * TAMANHO   : 0290 BYTES                                         *
      ******************************************************************
      * AREA DE ENTRADA:                                               *
      * ----------------                                               *
      * CCLIMT-RURAL         :                                         *
      ******************************************************************
      * AREA DE SAIDA:                                                 *
      * ----------------                                               *
      * CCLIMT-RURAL         :                                         *
      * CCLIMT-SIST          :                                         *
      * RCLIMT-SIST          :                                         *
      * DINIC-VGCIA-REG      : DATA DE INICIO DE VIGENCIA              *
      * DFIM-VGCIA-REG       : DATA DE FIM DE VIGENCIA                 *
      * CSIT-REG-RURAL       : SITUACAO DO REGISTRO DO RURAL           *
      * CUSUAR-INCL          : USUARIO DE INCLUSAO REGISTRO            *
      * HINCL-REG            : DATA E HORA INCLUSAO REGISTRO           *
      * CUSUAR-MANUT         : USUARIO DE MANUTENCAO DO REGISTRO       *
      * HMANUT-REG           : DATA E HORA DE MANUTENCAO DO REGISTRO   *
      ******************************************************************
       05 CRURWL0I-HEADER.
           10 CRURWG0I-COD-LAYOUT       PIC X(008)  VALUE 'CRURWG0I'.
           10 CRURWG0I-TAM-LAYOUT       PIC 9(005)  VALUE 00290.
       05 CRURWG0I-REGISTRO.
           10 CRURWG0I-BLOCO-ENTRADA.
             15 CRURWG0I-E-CCLIMT-RURAL           PIC 9(02).
           10 CRURWG0I-S-BLOCO-SAIDA.
             15 CRURWG0I-S-CCLIMT-RURAL           PIC 9(02).
             15 CRURWG0I-S-CCLIMT-SIST            PIC 9(02).
             15 CRURWG0I-S-RCLIMT-SIST            PIC X(80).
             15 CRURWG0I-S-DINIC-VGCIA-REG        PIC X(10).
             15 CRURWG0I-S-DFIM-VGCIA-REG         PIC X(10).
             15 CRURWG0I-S-CSIT-REG-RURAL         PIC X(01).
             15 CRURWG0I-S-CUSUAR-INCL            PIC X(09).
             15 CRURWG0I-S-HINCL-REG              PIC X(26).
             15 CRURWG0I-S-CUSUAR-MANUT           PIC X(09).
             15 CRURWG0I-S-HMANUT-REG             PIC X(26).
             15 CRURWG0I-S-NOME-USUARIO           PIC X(40).
             15 CRURWG0I-S-NOME-DEPTO             PIC X(60).
