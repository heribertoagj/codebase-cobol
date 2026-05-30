      ******************************************************************
      * NOME BOOK : I#BVVE8A                                           *
      * DESCRICAO : DADOS PARA MANETR TABELA BVVEB048                  *
      *             - PRODUTO CARTAO AGENDA FINANCEIRA                 *
      * DATA      : JUNHO DE 2019.                                     *
      * AUTOR     : ELIEZER SIQUEIRA                                   *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 99999 BYTES                                        *
      ******************************************************************
      *
        01 BVVE8A-REGISTRO.
           05 BVVE8A-DADOS-BVVE048.
              10 BVVE8A-A-CPRODT-AGNDA-FINCR PIC  9(005).
              10 BVVE8A-A-CCRDRA-ESTBL-COML  PIC  9(004).
              10 BVVE8A-A-CTPO-VISAO-BANDE   PIC  9(003).
              10 BVVE8A-A-CBANDE-VISAO       PIC  9(003).
              10 BVVE8A-A-CINDCD-ORD-PGTO    PIC  9(001).
              10 BVVE8A-A-IPRODT-AGNDA-FINCR PIC  X(020).
              10 BVVE8A-A-CINDCD-SIT-REG     PIC  X(001).
              10 BVVE8A-A-CUSUAR-INCL        PIC  X(009).
              10 BVVE8A-A-HINCL-REG          PIC  X(026).
              10 BVVE8A-A-CUSUAR-MANUT       PIC  X(009).
              10 BVVE8A-A-HMANUT-REG         PIC  X(026).
           05 BVVE8A-DADOS-SAIDA.
              10 BVVE8A-S-IREDZD-CRDRA-ESTBL PIC  X(030).
              10 BVVE8A-S-IBANDE-VISAO       PIC  X(030).
           05 BVVE8A-CONTROLE.
              10 BVVE8A-A-PROG-CHAMADOR      PIC  X(008).
              10 BVVE8A-A-FASE               PIC  9(002).
              10 BVVE8A-A-CUSUAR-LOGIN       PIC  X(009).
           05 BVVE8A-RETORNO.
              10 BVVE8A-S-CD-RETORNO         PIC  9(003).
              10 BVVE8A-S-MENSAGEM           PIC  X(079).
              10 BVVE8A-S-CPO-ERRO           PIC  X(025).
           05 BVVE8A-DEMAIS-DADOS.
              10 FILLER                      PIC  X(300).
