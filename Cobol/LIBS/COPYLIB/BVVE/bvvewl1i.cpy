      ******************************************************************
      * NOME BOOK : BVVEWL1I                                           *
      * DESCRICAO : BOOK DE INTERFACE ENTRE PGM COORDENADOR E FUNCIONAL*
      * DATA      : 10/09/2018                                         *
      * AUTOR     : WILSON RIBEIRO                                     *
      * EMPRESA   : PRIME IT/CD BRADESCO                               *
      * COMPONENTE:                                                    *
      * TAMANHO   : 1199 BYTES                                         *
      ************************** DESCRICAO *****************************
      *                                                                *
      * CCRDRA-ESTBL-COML..: COD.  DA CREDENCIADORA                    *
      * IREDZD-CRDRA-ESTBL.: DESC. REDUZIDA DA CREDENCIADORA           *
      * ICRDRA-ESTBL-COML..: DESC. DA CREDENCIADORA                    *
      *                                                                *
      ******************************************************************
           05 BVVEWL1I-HEADER.
             10 BVVEWL1I-COD-LAYOUT    PIC X(08)   VALUE 'BVVEWL1I'.
WIP001       10 BVVEWL1I-TAM-LAYOUT    PIC 9(05)   VALUE 1439.
           05 BVVEWL1I-REGISTRO.
             10 BVVEWL1I-BLOCO-ENTRADA.
               15 BVVEWL1I-E-MAX-OCORR             PIC 9(05).
             10 BVVEWL1I-BLOCO-PAGINACAO.
               15 BVVEWL1I-INDICADOR-PAGINACAO     PIC X(01).
               15 BVVEWL1I-CHAVE-INI.
                 20 BVVEWL1I-I-CCRDRA-ESTBL-COML   PIC 9(04).
               15 BVVEWL1I-CHAVE-FIM.
                 20 BVVEWL1I-F-CCRDRA-ESTBL-COML   PIC 9(04).
             10 BVVEWL1I-BLOCO-SAIDA.
               15 BVVEWL1I-S-MENSAGEM              PIC X(79).
               15 BVVEWL1I-S-NUM-CONSULTAS         PIC 9(05).
               15 BVVEWL1I-S-DADOS-SAIDA  OCCURS 0 TO 16 TIMES
                       DEPENDING ON BVVEWL1I-S-NUM-CONSULTAS.
                 20 BVVEWL1I-S-CCRDRA-ESTBL-COML   PIC  9(004).
                 20 BVVEWL1I-S-CCRDRA-ESTBL-COML-X REDEFINES
                    BVVEWL1I-S-CCRDRA-ESTBL-COML   PIC  X(004).
                 20 BVVEWL1I-S-IREDZD-CRDRA-ESTBL  PIC  X(015).
                 20 BVVEWL1I-S-ICRDRA-ESTBL-COML   PIC  X(049).
WIP001           20 BVVEWL1I-S-CCNPJ-CRDRA-ESTBL   PIC  9(009).
WIP001           20 BVVEWL1I-S-CFLIAL-CRDRA-ESTB   PIC  9(004).
WIP001           20 BVVEWL1I-S-CCTRL-CRDRA-ESTBL   PIC  9(002).
