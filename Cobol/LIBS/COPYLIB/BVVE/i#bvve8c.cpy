      ******************************************************************
      * NOME BOOK : I#BVVE8C                                           *
      * DESCRICAO : DADOS PARA MANTER TABELA BVVEB044                  *
      *             - BANDEIRAS EXTERNAS                               *
      * DATA      : JUNHO DE 2019.                                     *
      * AUTOR     : ELIEZER SIQUEIRA                                   *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 99999 BYTES                                        *
      ******************************************************************
      *
        01 BVVE8C-REGISTRO.
           05  BVVE8C-DADOS-BVVEB044.
              10 BVVE8C-A-CTPO-BANDE         PIC  9(003).
              10 BVVE8C-A-CBANDE             PIC  9(003).
              10 BVVE8C-A-CTPO-AMBTE         PIC  9(001).
              10 BVVE8C-A-CBANDE-EXTER       PIC  X(005).
              10 BVVE8C-A-CUSUAR-INCL        PIC  X(009).
              10 BVVE8C-A-HINCL-REG          PIC  X(026).
           05 BVVE8C-DADOS-SAIDA.
              10 BVVE8C-S-IBANDE             PIC  X(035).
           05 BVVE8C-CONTROLE.
              10 BVVE8C-A-CUSUAR-LOGIN       PIC  X(009).
              10 BVVE8C-A-PROG-CHAMADOR      PIC  X(008).
              10 BVVE8C-A-FASE               PIC  9(002).
              10 BVVE8C-RETORNO.
                 15 BVVE8C-S-CD-RETORNO      PIC  9(003).
                 15 BVVE8C-S-MENSAGEM        PIC  X(079).
                 15 BVVE8C-S-CPO-ERRO        PIC  X(025).
           05 BVVE8C-DEMAIS-DADOS.
              10 BVVE8C-A-CUSUAR-MANUT       PIC  X(009).
              10 BVVE8C-A-HMANUT-REG         PIC  X(026).
              10 BVVE8C-A-FILTRO.
                 15 BVVE8C-A-CBANDE-F        PIC  9(003).
              10 FILLER                      PIC  X(262).
