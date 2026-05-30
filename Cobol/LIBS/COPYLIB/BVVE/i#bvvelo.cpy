      ******************************************************************
      * NOME BOOK : I#BVVELO                                           *
      * DESCRICAO : ARQUIVO RESULTADO PROCESSAMENTO ARQ VARREDURA CIP  *
      *             ASC103 - SCG INFORMA SIT AUTORIZ DE ENVIO AGENDA   *
      * DATA      : 27/06/2019                                         *
      * AUTOR     : CARLOS OROFINO                                     *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 250 BYTES                                          *
      * -------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                   *
      * -------------------------------------------------------------- *
      * BVVELO-OPER-CADASTRO:        ACAO A SER APLICADA NA TABELA     *
      *                               I - INCLUIR CADASTRO             *
      *                               E - EXCLUIR CADASTRO             *
      *                               N - NENHUMA ACAO PARA CADASTRO   *
      * BVVELO-CTPO-ARQ-AUTRZ:       TIPO DE ARQUIVO                   *
      *                               VALOR FIXO 3 PARA ASCG0103       *
      * BVVELO-CINDCD-CNPJ-BASE:     INDICADOR CNPJ BASE               *
      *                               VALOR FIXO N PARA ASCG0103       *
      *                               NÃO SE APLICA                    *
      * BVVELO-CSIT-PROCM-AUTRZ:     COD SITUACAO PROCESSAMENTO        *
      *                               VALOR FIXO 4 PARA ASCG103        *
      *                               4 - SOLIC RECEBIDA DA CIP        *
      * BVVELO-NCTRL-AUTRZ-AGNDA:    NAO VEM PREENCHIDO                *
      *                               DEVE SER OBTIDO AO SE INCLUIR    *
      *                               NA TABELA BVVEB045               *
      ******************************************************************
       01  BVVELO-ASCG103.
           05 BVVELO-ENVIO-AGENDA.
              10 BVVELO-OPER-CADASTRO        PIC  X(001).
           05 BVVELO-COMPL-ATLZ-TAB.
              10 BVVELO-CCNPJ-CPF-AUTRZ      PIC S9(09)V USAGE COMP-3.
              10 BVVELO-CFLIAL-CNPJ-AUTRZ    PIC S9(05)V USAGE COMP-3.
              10 BVVELO-CCTRL-CPF-AUTRZ      PIC S9(02)V USAGE COMP-3.
              10 BVVELO-CBCO                 PIC S9(03)V USAGE COMP-3.
              10 BVVELO-CAG-BCRIA            PIC S9(05)V USAGE COMP-3.
              10 BVVELO-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
              10 BVVELO-CCRDRA-ESTBL-COML    PIC S9(04)V USAGE COMP-3.
              10 BVVELO-CTPO-VISAO-BANDE     PIC S9(03)V USAGE COMP-3.
              10 BVVELO-CBANDE-VISAO         PIC S9(03)V USAGE COMP-3.
              10 BVVELO-CTPO-ARQ-AUTRZ       PIC S9(01)V USAGE COMP-3.
              10 BVVELO-CID-TPO-PSSOA        PIC  X(01).
              10 BVVELO-CINDCD-DOMCL-BCRIO   PIC  X(01).
              10 BVVELO-CINDCD-CNPJ-BASE     PIC  X(01).
              10 BVVELO-CSIT-PROCM-AUTRZ     PIC S9(01)V USAGE COMP-3.
              10 BVVELO-CSIT-AUTRZ-AGNDA     PIC  X(01).
              10 BVVELO-CCTRL-RQUIS-INSTC    PIC  X(20).
              10 BVVELO-CID-INCL-AUTRZ       PIC  X(20).
              10 BVVELO-CID-CANCT-AUTRZ      PIC  X(20).
              10 BVVELO-CID-AUTRZ-AGNDA      PIC  X(20).
              10 BVVELO-NCTRL-AUTRZ-AGNDA    PIC S9(11)V USAGE COMP-3.
              10 BVVELO-HENVIO-AUTRZ-AGNDA   PIC  X(26).
              10 BVVELO-HRECEB-AUTRZ-AGNDA   PIC  X(26).
              10 BVVELO-CERRO-MSGEM-SIST     PIC  X(08).
              10 BVVELO-CUSUAR-INCL          PIC  X(09).
              10 BVVELO-CUSUAR-MANUT         PIC  X(07).
              10 BVVELO-HMANUT-REG           PIC  X(26).
              10 FILLER                      PIC  X(26).
