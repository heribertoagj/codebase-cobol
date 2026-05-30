      ******************************************************************
      * NOME BOOK : I#BVVELE                                           *
      * DESCRICAO : ARQUIVO VARREDURA CIP                              *
      *             ASC103 - SCG INFORMA SIT AUTORIZ DE ENVIO AGENDA   *
      * DATA      : 27/06/2019                                         *
      * AUTOR     : CARLOS OROFINO                                     *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 250 BYTES                                          *
      * -------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                   *
      * -------------------------------------------------------------- *
      *                                                                *
      * BVVELE-TPO-REG            :  TIPO DE REGISTRO                  *
      *                              0 - HEADER    1 - DETALHE         *
      *                                                                *
      * BVVELE-NR-ID-SCG-AUTRZ-ENV:  NRO IDENTIF SCG DA AUTORIZACAO    *
      *                              ENVIO DE AGENDA                   *
      *                                                                *
      * BVVELE-ISPB-IF:              IDENTIFICADOR PARTICIPANTE JUNTO  *
      *                              AO BACEN                          *
      *                              O ISPB DO EMISSOR, DO LADO DAS    *
      *                              INSTIT.FINANC, REPRESENTA O CNPJ  *
      *                              BASE DO PARTICIPANTE PRINCIPAL    *
      *                                                                *
      * BVVELE-CNPJ-CREDDR:          CNPJ CREDENCIADOR                 *
      *                              FORMATO 'NNNNNNNNCCCCDD'          *
      *                                                                *
      * BVVELE-TP-PSSOA CLI:         TIPO PESSOA DO CLIENTE            *
      *                              F - FISICA                        *
      *                              J - JURIDICA                      *
      * BVVELE-CPF-CNPJ-CLIENTE:     CPF-CNPJ DO CLIENTE               *
      *                              FORMATO 'NNNNNNNNNCCBBB'          *
      *                              CNPJ DO CLIENTE                   *
      *                              FORMATO 'NNNNNNNNFFFFCC'          *
      *                                                                *
      * BVVELE-CD-INSTT-ARRANJO-PGTO:COD INSTITUIDOR ARRANJO PGTO      *
      *                                                                *
      * BVVELE-INDR-DOMCL:           INDICA SE INSTITUICAO POSSUI DOMI-*
      *                              CILIO DO CLIENTE                  *
      *                              S - SIM                           *
      *                              N - NÃO                           *
      *                                                                *
      * BVVELE-SIT-AUTORZ-ENV-AGENDA:SIT.AUTORIZACAO ENVIO AGENDA      *
      *                              A - ATIVO                         *
      *                              C - CANCELADO                     *
      *                                                                *
      * BVVELE-DT-HR-ENV-AGDA:       DATA-HORA SITUAÇÃO AUTORIZ        *
      *                              FORMATO 'AAAA-MM-DDTHH:MM:SS'     *
      *                              DATA - AAAA-MM-DD                 *
      *                              HORA - THH:MM:SS                  *
      ******************************************************************
       01  BVVELE-ASCG103.
           05 BVVELE-ENVIO-AGENDA.
              10 BVVELE-TPO-REG                      PIC  X(001).
              10 BVVELE-TPO-CTR                      PIC  X(001).
              10 BVVELE-NR-ID-SCG-AUTRZ-ENV          PIC  X(020).
              10 BVVELE-ISPB-IF                      PIC  X(008).
              10 BVVELE-CNPJ-CREDDR.
                 15 BVVELE-CNPJ-CRED-BASE            PIC  9(008).
                 15 BVVELE-CNPJ-CRED-FILIAL          PIC  9(004).
                 15 BVVELE-CNPJ-CRED-CTR             PIC  9(002).
              10 BVVELE-TP-PSSOA-CLI                 PIC  X(001).
              10 BVVELE-CNPJ-CPF-CLI                 PIC  X(014).
              10 BVVELE-CNPJ                         REDEFINES
                                             BVVELE-CNPJ-CPF-CLI.
                 15 BVVELE-CNPJ-BASE                 PIC  9(008).
                 15 BVVELE-CNPJ-FILIAL               PIC  9(004).
                 15 BVVELE-CNPJ-CTR                  PIC  9(002).
              10 BVVELE-CPF                          REDEFINES
                                             BVVELE-CNPJ-CPF-CLI.
                 15 BVVELE-CPF-NUM                   PIC  9(009).
                 15 BVVELE-CPF-CTR                   PIC  9(002).
                 15 BVVELE-BRANCOS                   PIC  X(003).
              10 BVVELE-COD-INSTI-ARRAJ-PGTO         PIC  X(003).
              10 BVVELE-INDR-DOMCL                   PIC  X(001).
              10 BVVELE-SIT-AUTC-ENV-AGDA            PIC  X(001).
              10 BVVELE-DT-HR-ENV-AGDA.
                 15 BVVELE-DATA-ENV-AGDA             PIC  X(010).
                 15 BVVELE-HORA-ENV-AGDA             PIC  X(009).
              10 FILLER                              PIC  X(167).
