      ******************************************************************
      * NOME BOOK : I#BVVELN                                           *
      * DESCRICAO : ARQUIVO VARREDURA CIP - INCONSISTENTES             *
      *             ASC103 - SCG INFORMA SIT AUTORIZ DE ENVIO AGENDA   *
      * DATA      : 27/06/2019                                         *
      * AUTOR     : CARLOS OROFINO                                     *
      * EMPRESA   : PRIME INFORMATICA                                  *
      * TAMANHO   : 120 BYTES             BVVELE-NR-ID-SCG-AUTRZ-ENV   *
      * -------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                   *
      * -------------------------------------------------------------- *
      *                                                                *
      * BVVELE-NR-ID-SCG-AUTRZ-ENV:  NRO IDENTIF SCG DA AUTORIZACAO    *
      *                              ENVIO DE AGENDA                   *
      *                                                                *
      * BVVELN-ISPB-IF:              IDENTIFICADOR PARTICIPANTE JUNTO  *
      *                              AO BACEN                          *
      *                              O ISPB DO EMISSOR, DO LADO DAS    *
      *                              INSTIT.FINANC, REPRESENTA O CNPJ  *
      *                              BASE DO PARTICIPANTE PRINCIPAL    *
      *                                                                *
      * BVVELN-CNPJ-CREDDR:          CNPJ CREDENCIADOR                 *
      *                              FORMATO 'NNNNNNNNCCCCDD'          *
      *                                                                *
      * BVVELN-TP-PSSOA CLI:         TIPO PESSOA DO CLIENTE            *
      *                               F - FISICA                       *
      *                               J - JURIDICA                     *
      * BVVELN-CPF-CNPJ-CLIENTE:     CPF-CNPJ DO CLIENTE               *
      *                               FORMATO 'NNNNNNNNNCCBBB'         *
      *                               CNPJ DO CLIENTE                  *
      *                               FORMATO 'NNNNNNNNFFFFCC'         *
      *                                                                *
      * BVVELN-CD-INSTT-ARRANJO-PGTO: COD INSTITUIDOR ARRANJO PGTO     *
      *                                                                *
      * BVVELE-INDR-DOMCL:           INDICA SE INSTITUICAO POSSUI DOMI-*
      *                              CILIO DO CLIENTE                  *
      *                               S - SIM                          *
      *                               N - NÃO                          *
      *                                                                *
      * BVVELN-SIT-AUTORZ-ENV-AGENDA: SIT.AUTORIZACAO ENVIO AGENDA     *
      *                               A - ATIVO                        *
      *                               C - CANCELADO                    *
      *                                                                *
      * BVVELE-DT-HR-ENV-AGDA:       DATA-HORA SITUAÇÃO AUTORIZ        *
      *                               FORMATO 'AAAA-MM-DDTHH:MM:SS'    *
      *                               DATA - AAAA-MM-DD                *
      *                               HORA - THH:MM:SS                 *
      ******************************************************************
       01  BVVELN-ASCG103.
           05 BVVELN-ENVIO-AGENDA.
              10 BVVELN-ERRO-01                      PIC  X(001).
              10 BVVELN-NR-ID-SCG-AUTRZ-ENV          PIC  X(020).
              10 BVVELN-ERRO-02                      PIC  X(001).
              10 BVVELN-ISPB-IF                      PIC  X(008).
              10 BVVELN-ERRO-03                      PIC  X(001).
              10 BVVELN-CNPJ-CREDDR.
                 15 BVVELN-BASE-CNPJ-CREDENC         PIC  9(008).
                 15 BVVELN-FILIAL-CNPJ-CREDENC       PIC  9(004).
                 15 BVVELN-DIG-CNPJ-CREDENC          PIC  9(002).
              10 BVVELN-ERRO-04                      PIC  X(001).
              10 BVVELN-TP-PSSOA-CLI                 PIC  X(001).
              10 BVVELN-ERRO-05                      PIC  X(001).
              10 BVVELN-CNPJ-CPF-CLI                 PIC  X(014).
              10 BVVELN-ERRO-06                      PIC  X(001).
              10 BVVELN-COD-INSTI-ARRAJ-PGTO         PIC  X(003).
              10 BVVELN-ERRO-07                      PIC  X(001).
              10 BVVELN-INDR-DOMCL                   PIC  X(001).
              10 BVVELN-ERRO-08                      PIC  X(001).
              10 BVVELN-SIT-AUTC-ENV-AGDA            PIC  X(001).
              10 BVVELN-DT-HR-ENV-AGDA.
                 15 BVVELE-DATA-ENV-AGDA             PIC  X(010).
                 15 BVVELE-HORA-ENV-AGDA             PIC  X(009).
              10 FILLER                              PIC  X(045).
