      ******************************************************************
      * NOME BOOK : BVVEWL2I                                           *
      * DESCRICAO : BOOK DE INTERFACE ENTRE PGM COORDENADOR E FUNCIONAL*
      * DATA      : 10/09/2018                                         *
      * AUTOR     : WILSON RIBEIRO                                     *
      * EMPRESA   : PRIME IT/CD BRADESCO                               *
      * COMPONENTE:                                                    *
      * TAMANHO   : 653 BYTES                                          *
      ************************** DESCRICAO *****************************
      *                                                                *
      * CARRJO-PGTO.....:    COD.  DA CREDENCIADORA                    *
      * IARRJO-PGTO.....:    DESC. REDUZIDA DA CREDENCIADORA           *
      * CTPO-ARRJO-PGTO.:    DESC. DA CREDENCIADORA                    *
      *                                                                *
      ******************************************************************
           05 BVVEWL2I-HEADER.
             10 BVVEWL2I-COD-LAYOUT    PIC X(08)   VALUE 'BVVEWL2I'.
             10 BVVEWL2I-TAM-LAYOUT    PIC 9(05)   VALUE 653.
           05 BVVEWL2I-REGISTRO.
             10 BVVEWL2I-BLOCO-ENTRADA.
               15 BVVEWL2I-E-MAX-OCORR             PIC 9(05).
             10 BVVEWL2I-BLOCO-PAGINACAO.
               15 BVVEWL2I-INDICADOR-PAGINACAO     PIC X(01).
               15 BVVEWL2I-CHAVE-INI.
                 20 BVVEWL2I-I-CARRJO-PGTO         PIC 9(03).
               15 BVVEWL2I-CHAVE-FIM.
                 20 BVVEWL2I-F-CARRJO-PGTO         PIC 9(03).
             10 BVVEWL2I-BLOCO-SAIDA.
               15 BVVEWL2I-S-MENSAGEM              PIC X(79).
               15 BVVEWL2I-S-NUM-CONSULTAS         PIC 9(05).
               15 BVVEWL2I-S-DADOS-SAIDA  OCCURS 0 TO 16 TIMES
                       DEPENDING ON BVVEWL2I-S-NUM-CONSULTAS.
                 20 BVVEWL2I-S-CARRJO-PGTO         PIC  9(003).
                 20 BVVEWL2I-S-IARRJO-PGTO         PIC  X(030).
                 20 BVVEWL2I-S-CTPO-ARRJO-PGTO     PIC  X(001).
