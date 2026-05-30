      ******************************************************************
      * NOME BOOK : BVVEWL4I                                           *
      * DESCRICAO : BOOK DE INTERFACE ENTRE PGM COORDENADOR E FUNCIONAL*
      * DATA      : 19/10/2018                                         *
      * AUTOR     : GISLAINE B LIMA                                    *
      * EMPRESA   : CD CURITIBA                                        *
      * TAMANHO   : 1279 BYTES                                         *
      ************************** DESCRICAO *****************************
      *                                                                *
      * CCRDRA-ESTBL-COML..: COD. DA CREDENCIADORA                     *
      * IREDZD-CRDRA-ESTBL : DESCRICAO CREDENCIADORA                   *
      * CARRJO-PGTO........: COD. ARRANJO DE PAGAMENTO                 *
      * IARRJO_PGTO........: DESCRICAO ARRANJO DE PAGAMENTO            *
      * NPRFIL-CRDRA-PGTO..: NUMERO DO PERFIL CADASTRADO               *
      * CIDTFD-LCTO........: COD. LANCAMENTO                           *
      *                                                                *
      ******************************************************************
           05 BVVEWL4I-HEADER.
             10 BVVEWL4I-COD-LAYOUT    PIC X(08)   VALUE 'BVVEWL4I'.
             10 BVVEWL4I-TAM-LAYOUT    PIC 9(05)   VALUE 1279.
           05 BVVEWL4I-REGISTRO.
             10 BVVEWL4I-BLOCO-ENTRADA.
               15 BVVEWL4I-E-MAX-OCORR             PIC 9(05).
             10 BVVEWL4I-BLOCO-PAGINACAO.
               15 BVVEWL4I-INDICADOR-PAGINACAO     PIC X(01).
               15 BVVEWL4I-CHAVE-INI.
                 20 BVVEWL4I-I-CCRDRA-ESTBL-COML   PIC 9(04).
MY1018           20 BVVEWL4I-I-CARRJO-PGTO         PIC 9(03).
.                20 BVVEWL4I-I-NPRFIL-CRDRA-PGTO   PIC 9(01).
               15 BVVEWL4I-CHAVE-FIM.
                 20 BVVEWL4I-F-CCRDRA-ESTBL-COML   PIC 9(04).
MY1018           20 BVVEWL4I-F-CARRJO-PGTO         PIC 9(03).
.                20 BVVEWL4I-F-NPRFIL-CRDRA-PGTO   PIC 9(01).

             10 BVVEWL4I-BLOCO-SAIDA.
               15 BVVEWL4I-S-MENSAGEM              PIC X(79).
               15 BVVEWL4I-S-NUM-CONSULTAS         PIC 9(05).
               15 BVVEWL4I-S-DADOS-SAIDA  OCCURS 0 TO 16 TIMES
                       DEPENDING ON BVVEWL4I-S-NUM-CONSULTAS.
                 20 BVVEWL4I-S-CCRDRA-ESTBL-COML   PIC  9(004).
                 20 BVVEWL4I-S-CCRDRA-ESTBL-COML-X REDEFINES
                    BVVEWL4I-S-CCRDRA-ESTBL-COML   PIC  X(004).
                 20 BVVEWL4I-S-IREDZD-CRDRA-ESTBL  PIC  X(030).
                 20 BVVEWL4I-S-CARRJO-PGTO         PIC  9(003).
                 20 BVVEWL4I-S-CARRJO-PGTO-X       REDEFINES
                    BVVEWL4I-S-CARRJO-PGTO         PIC  X(003).
                 20 BVVEWL4I-S-IARRJO-PGTO         PIC  X(030).
                 20 BVVEWL4I-S-NPRFIL-CRDRA-PGTO   PIC  9(001).
                 20 BVVEWL4I-S-NPRFIL-CRDRA-PGTO-X REDEFINES
                    BVVEWL4I-S-NPRFIL-CRDRA-PGTO   PIC  X(001).
                 20 BVVEWL4I-S-CIDTFD-LCTO         PIC  9(005).
                 20 BVVEWL4I-S-CIDTFD-LCTO-X       REDEFINES
                    BVVEWL4I-S-CIDTFD-LCTO         PIC  X(005).
