      ******************************************************************
      * NOME BOOK : I#BVVE8D                                           *
      * DESCRICAO : DADOS PARA MANTER TABELA BVVEB044                  *
      *             - BANDEIRAS EXTERNAS                               *
      * DATA      : JUNHO DE 2019.                                     *
      * AUTOR     : ELIEZER SIQUEIRA                                   *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 99999 BYTES                                        *
      ******************************************************************
      *
        01 BVVE8D-REGISTRO.
           05 BVVE8D-DADOS-ENTRADA.
              10 BVVE8D-E-CBANDE-VISAO           PIC  9(003).
           05 BVVE8D-CHAVE-PAGINACAO.
              10 BVVE8D-PRI-CHAVE.
                 15 BVVE8D-PRI-CBANDE            PIC  9(003).
                 15 BVVE8D-PRI-CTPO-AMBTE        PIC  9(001).
              10 BVVE8D-ULT-CHAVE.
                 15 BVVE8D-ULT-CBANDE            PIC  9(003).
                 15 BVVE8D-ULT-CTPO-AMBTE        PIC  9(001).
           05 BVVE8D-CONTROLE.
              10 BVVE8D-A-PROG-CHAMADOR          PIC  X(008).
              10 BVVE8D-A-FASE                   PIC  9(002).
              10 BVVE8D-A-PAGINA                 PIC  9(003).
              10 BVVE8D-A-IND-PAGINACAO          PIC  X(001).
              10 BVVE8D-A-CUSUAR-LOGIN           PIC  X(009).
           05 BVVE8D-RETORNO.
              15 BVVE8D-S-CD-RETORNO             PIC  9(003).
              15 BVVE8D-S-MENSAGEM               PIC  X(079).
              15 BVVE8D-S-CPO-ERRO               PIC  X(025).
           05 FILLER                             PIC  X(300).
           05 BVVE8D-BLOCO-LISTA.
              10 BVVE8D-E-QTDD-OCC               PIC  9(002).
              10 BVVE8D-S-QTDD-OCC               PIC  9(002).
              10 BVVE8D-A-FIM-DADOS              PIC  X(001).
              10 BVVE8D-OCORRENCIAS              OCCURS  16 TIMES.
                 15 BVVE8D-S-CBANDE              PIC  9(003).
                 15 BVVE8D-S-IBANDE              PIC  X(035).
                 15 BVVE8D-S-CBANDE-EXTER        PIC  X(005).
                 15 BVVE8D-S-CTPO-AMBTE          PIC  9(001).
                 15 BVVE8D-FILLER                PIC  X(100).
