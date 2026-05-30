      ******************************************************************
      * NOME BOOK : BVVEWL3I                                           *
      * DESCRICAO : BOOK DE INTERFACE ENTRE PGM COORDENADOR E FUNCIONAL*
      * DATA      : 10/09/2018                                         *
      * AUTOR     : WILSON RIBEIRO                                     *
      * EMPRESA   : PRIME IT/CD BRADESCO                               *
      * COMPONENTE:                                                    *
      * TAMANHO   : 1383 BYTES                                         *
      ************************** DESCRICAO *****************************
      *                                                                *
      * CCRDRA-ESTBL-COML..: COD.   DA CREDENCIADORA                   *
      * IREDZD-CRDRA-ESTBL.: DESC.  DA CREDENCIADORA                   *
      * CCNPJ-CRDRA-ESTBL..: CNPJ   DA CREDENCIADORA                   *
      * CFLIAL-CRDRA-ESTBL.: FLIAL  DA CREDENCIADORA                   *
      * CCTRL-CRDRA-ESTBL..: CTRL   DA CREDENCIADORA                   *
      * NPRFIL-CRDRA-PGTO..: NPRFIL DA CREDENCIADORA                   *
      * NSEQ-CONTR-NEGOC...: NSEQ DO CONTRATO                          *
      * CAG-BCRIA-CLI......: AGENCIA DO CLIENTE                        *
      * NCTA-BCRIA.........: CONTA DO CLIENTE                          *
      *                                                                *
      ******************************************************************
           05 BVVEWL3I-HEADER.
             10 BVVEWL3I-COD-LAYOUT    PIC X(08)   VALUE 'BVVEWL3I'.
             10 BVVEWL3I-TAM-LAYOUT    PIC 9(05)   VALUE 1383.
           05 BVVEWL3I-REGISTRO.
             10 BVVEWL3I-BLOCO-ENTRADA.
               15 BVVEWL3I-E-MAX-OCORR             PIC 9(05).
               15 BVVEWL3I-INDICADOR-PAGINACAO     PIC X(01).
             10 BVVEWL3I-CHAVE-INI.
               15 BVVEWL3I-I-CCRDRA-ESTBL-COML     PIC 9(04).
               15 BVVEWL3I-I-NPRFIL-CRDRA-PGTO     PIC 9(10).
               15 BVVEWL3I-I-NSEQ-CONTR-NEGOC      PIC 9(10).
             10 BVVEWL3I-CHAVE-FIM.
               15 BVVEWL3I-F-CCRDRA-ESTBL-COML     PIC 9(04).
               15 BVVEWL3I-F-NPRFIL-CRDRA-PGTO     PIC 9(10).
               15 BVVEWL3I-F-NSEQ-CONTR-NEGOC      PIC 9(10).
             10 BVVEWL3I-BLOCO-SAIDA.
               15 BVVEWL3I-S-MENSAGEM              PIC  X(79).
               15 BVVEWL3I-S-NUM-CONSULTAS         PIC  9(05).
               15 BVVEWL3I-S-DADOS-SAIDA   OCCURS 0 TO 16 TIMES
                        DEPENDING ON BVVEWL3I-S-NUM-CONSULTAS.
                 20 BVVEWL3I-S-CCRDRA-ESTBL-COML   PIC  9(004).
                 20 BVVEWL3I-S-IREDZD-CRDRA-ESTBL  PIC  X(030).
                 20 BVVEWL3I-S-CCNPJ-CRDRA-ESTBL   PIC  9(009).
                 20 BVVEWL3I-S-CFLIAL-CRDRA-ESTBL  PIC  9(004).
                 20 BVVEWL3I-S-CCTRL-CRDRA-ESTBL   PIC  9(002).
                 20 BVVEWL3I-S-NPRFIL-CRDRA-PGTO   PIC  9(001).
                 20 BVVEWL3I-S-NSEQ-CONTR-NEGOC    PIC  9(010).
                 20 BVVEWL3I-S-CAG-BCRIA-CLI       PIC  9(005).
                 20 BVVEWL3I-S-NCTA-BCRIA          PIC  9(012).
