      ****************************************************************
      * NOME BOOK : UORGW22B                                         *
      * DESCRICAO : INTERFACE ENTRE O SERVICO COORDENADOR UORG122B E *
      *             O FUNCIONAL UORG122B - CONSULTA UNID. ORGANIZ.   *
      ********************   OBSERVACAO       ************************
      * ESTE BOOK FOI CRIADO COM BASE NO BOOK UORGW22B SOMENTE       *
      * INCLUINDO MAIS CAMPOS NO RETORNO                             *
      ********************   OBSERVACAO       ************************
      * DATA      : 01/06/2010                                       *
      * AUTOR     : GPTI                                             *
      * EMPRESA   : GPTI                                             *
      * GRUPO     : 17 - GISS                                        *
      * COMPONENTE: UORG - UNIDADE ORGANIZACIONAL                    *
      * TAMANHO   : FIXO - UORGW22B-TAM-LAYOUT                       *
      ****************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO          *
      ****************************************************************
4SYS01* 15/08/2025 ULISSES SOARES    CNPJ ALFANUMERICO               *
      ****************************************************************
      *                                                              *
      * UORGW22B-TAM-LAYOUT         = TAMANHO DO REGISTRO            *
      *                                                              *
      * BLOCO DE ENTRADA                                             *
      * UORGW22B-E-CPSSOA-JURID     = CODIGO DA PESSOA JURIDICA(CLUB)*
      * UORGW22B-E-NSEQ-UND-ORGNZ   = NUMERO SEQUENCIAL DA UORG      *
      *                                                              *
      * BLOCO DE SAIDA                                               *
      * DADOS REFERENTES A UNIDADE ORGANIZACIONAL (UORGB007)         *
      * UORGW22B-07-CPSSOA-UND-ORGNZ= CODIGO DA PESSOA UNID.ORG(CLUB)*
      * UORGW22B-07-IUND-ORGNZ      = NOME DA UNIDADE ORGANIZACIONAL *
      * UORGW22B-07-CUND-ORGNZ      = CODIGO DA UNIDADE ORGANIZACION.*
      * UORGW22B-07-CDIG-UND-ORGNZ  = DIGITO UNIDADE ORGANIZACIONAL  *
      * UORGW22B-10-CCNPJ_DEPDC     = NUMERO CNPJ DA DEPENDENCIA     *
      * UORGW22B-10-CLJ_CNPJ_DEPDC  = CONTROLE CNPJ DA DEPENDENCIA   *
      * UORGW22B-10-CDIG_CNPJ_DEPDC = DIGITO CNPJ DA DEPENDENCIA     *
      ****************************************************************
           05 UORGW22B-HEADER.
              10 UORGW22B-COD-LAYOUT      PIC X(008) VALUE 'UORGW22B'.
              10 UORGW22B-TAM-LAYOUT      PIC 9(005) VALUE      00110.
           05 UORGW22B-REGISTRO.
              10 UORGW22B-BLOCO-ENTRADA.
                 15 UORGW22B-E-NSEQ-UND-ORGNZ      PIC 9(008).
              10 UORGW22B-BLOCO-SAIDA.
                 15 UORGW22B-07-CPSSOA-UND-ORGNZ   PIC 9(010).
                 15 UORGW22B-07-IUND-ORGNZ         PIC X(060).
                 15 UORGW22B-07-CUND-ORGNZ         PIC 9(006).
                 15 UORGW22B-07-CDIG-UND-ORGNZ     PIC X(001).
                 15 UORGW22B-10-CCNPJ-DEPDC        PIC 9(009).
4SYS01           15 UORGW22B-10-CCNPJ-DEPDC-ST     REDEFINES
4SYS01              UORGW22B-10-CCNPJ-DEPDC        PIC X(009).
                 15 UORGW22B-10-CLJ-CNPJ-DEPDC     PIC 9(004).
4SYS01           15 UORGW22B-10-CLJ-CNPJ-DEPDC-ST  REDEFINES
4SYS01              UORGW22B-10-CLJ-CNPJ-DEPDC     PIC X(004).
                 15 UORGW22B-10-CDIG-CNPJ-DEPDC    PIC X(002).
