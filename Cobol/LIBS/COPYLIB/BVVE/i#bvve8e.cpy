      ******************************************************************
      * NOME BOOK : I#BVVE8E                                           *
      * DESCRICAO : DADOS PARA MANTER TABELA BVVEB044                  *
      *             - BANDEIRAS EXTERNAS                               *
      * DATA      : JUNHO DE 2019.                                     *
      * AUTOR     : ELIEZER SIQUEIRA                                   *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 99999 BYTES                                        *
      ******************************************************************
      *
        01 BVVE8E-REGISTRO.
           05  BVVE8E-DADOS-BVVEB044.
              10 BVVE8E-A-CTPO-BANDE         PIC  9(003).
              10 BVVE8E-A-CBANDE             PIC  9(003).
              10 BVVE8E-A-CTPO-AMBTE         PIC  9(001).
              10 BVVE8E-A-CBANDE-EXTER       PIC  X(005).
              10 BVVE8E-A-CUSUAR-INCL        PIC  X(009).
              10 BVVE8E-A-HINCL-REG          PIC  X(026).
           05 BVVE8E-CHAVE-PAGINACAO.
              10 BVVE8E-PRI-CHAVE.
                 15 BVVE8E-PRI-CBANDE        PIC  9(003).
              10 BVVE8E-ULT-CHAVE.
                 15 BVVE8E-ULT-CBANDE        PIC  9(003).
           05 BVVE8E-DADOS-SAIDA.
              10 BVVE8E-S-IBANDE             PIC  X(035).
           05 BVVE8E-CONTROLE.
              10 BVVE8E-A-CUSUAR-LOGIN       PIC  X(009).
              10 BVVE8E-A-PROG-CHAMADOR      PIC  X(008).
              10 BVVE8E-A-TELA-CONTROLE      PIC  X(008).
              10 BVVE8E-A-IND-PAGINACAO      PIC  X(001).
              10 BVVE8E-A-PAGINA             PIC  9(003).
              10 BVVE8E-A-FASE               PIC  9(002).
              10 BVVE8E-RETORNO.
                 15 BVVE8E-S-CD-RETORNO      PIC  9(003).
                 15 BVVE8E-S-MENSAGEM        PIC  X(079).
                 15 BVVE8E-S-CPO-ERRO        PIC  X(025).
           05 BVVE8E-DEMAIS-DADOS.
              10 BVVE8E-A-CUSUAR-MANUT       PIC  X(009).
              10 BVVE8E-A-HMANUT-REG         PIC  X(026).
              10 BVVE8E-A-FILTRO.
                 15 BVVE8E-A-CBANDE-F        PIC  9(003).
              10 FILLER                      PIC  X(262).
           05 BVVE8E-BLOCO-LISTA.
              10 BVVE8E-E-QTDD-OCC           PIC  9(002).
              10 BVVE8E-S-QTDD-OCC           PIC  9(002).
              10 BVVE8E-A-FIM-DADOS          PIC  X(001).
              10 BVVE8E-OCORRENCIAS          OCCURS  13 TIMES.
                 15 BVVE8E-S-CBANDE          PIC  9(003).
                 15 BVVE8E-S-IBANDE-LISTA    PIC  X(050).
                 15 BVVE8E-FILLER            PIC  X(100).
