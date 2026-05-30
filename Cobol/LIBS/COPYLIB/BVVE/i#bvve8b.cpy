      ******************************************************************
      * NOME BOOK : I#BVVE8B                                           *
      * DESCRICAO : DADOS PARA MANETR TABELA BVVEB001                  *
      *             - PRODUTO CARTAO AGENDA FINANCEIRA                 *
      * DATA      : JUNHO DE 2019.                                     *
      * AUTOR     : ELIEZER SIQUEIRA                                   *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 99999 BYTES                                        *
      ******************************************************************
      *
        01 BVVE8B-REGISTRO.
           05 BVVE8B-DADOS.
              10 BVVE8B-A-CPRODT-AGNDA-FINCR PIC  9(005).
              10 BVVE8B-A-CCRDRA-ESTBL-COML  PIC  9(004).
              10 BVVE8B-A-CTPO-VISAO-BANDE   PIC  9(003).
              10 BVVE8B-A-CBANDE-VISAO       PIC  9(003).
           05 BVVE8B-CHAVE-PAGINACAO.
              10 BVVE8B-PRI-CHAVE.
                 15 BVVE8B-PRI-CPROD         PIC  9(005).
                 15 BVVE8B-PRI-CESTAB        PIC  9(004).
                 15 BVVE8B-PRI-CBAND         PIC  9(003).
              10 BVVE8B-ULT-CHAVE.
                 15 BVVE8B-ULT-CPROD         PIC  9(005).
                 15 BVVE8B-ULT-CESTAB        PIC  9(004).
                 15 BVVE8B-ULT-CBAND         PIC  9(003).
           05 BVVE8B-FILTRO-LISTA.
              10 BVVE8B-E-PRODUTO            PIC  9(005).
              10 BVVE8B-E-CREDENCIADORA      PIC  9(004).
              10 BVVE8B-E-BANDEIRA           PIC  9(003).
           05 BVVE8B-CONTROLE.
              10 BVVE8B-A-IND-PAGINACAO      PIC  X(001).
              10 BVVE8B-A-PAGINA             PIC  9(002).
              10 BVVE8B-A-PROG-CHAMADOR      PIC  X(008).
              10 BVVE8B-A-FASE               PIC  9(002).
              10 BVVE8B-A-CUSUAR-LOGIN       PIC  X(009).
              10 BVVE8B-A-CURSOR-ASC         PIC  9(002).
              10 BVVE8B-A-CURSOR-DESC        PIC  9(002).
           05 BVVE8B-RETORNO.
                 15 BVVE8B-S-CD-RETORNO      PIC  9(003).
                 15 BVVE8B-S-MENSAGEM        PIC  X(079).
                 15 BVVE8B-S-CPO-ERRO        PIC  X(025).
           05 FILLER                         PIC  X(100).
           05 BVVE8B-LISTA.
              10 BVVE8B-E-QTDD-OCC           PIC  9(002).
              10 BVVE8B-S-QTDD-OCC           PIC  9(002).
              10 BVVE8B-A-FIM-DADOS          PIC  X(001).
              10 BVVE8B-OCORRENCIAS       OCCURS     16 TIMES.
                 15 BVVE8B-S-CPROD           PIC  9(005).
                 15 BVVE8B-S-DESCRICAO       PIC  X(020).
                 15 BVVE8B-S-SITUACAO        PIC  X(007).
                 15 BVVE8B-S-FILLER          PIC  X(100).
