### 📘 Project Rules – PROMPT TRAE: Alteração de Campos CNPJ e FILIAL

#### 🎯 Objetivo
Migrar campos CNPJ e FILIAL de formato numérico para alfanumérico nos programas BVVE e DCOM, respeitando os padrões sintáticos COBOL e regras de consistência.

---

#### 🧱 1. Formatação COBOL

- **Colunas 1–6**: prefixo do programa (ex: `4S2511`)
- **Coluna 7**: `*` para comentários
- **Procedure Division**: instruções iniciam na coluna 20
- **Working-Storage Section**:
  - Nível 01 → coluna 8
  - Níveis inferiores → coluna 20

---

#### 🧬 2. Escopo das Alterações

- Ajuste nos copybooks
- Ajuste de formato das variáveis
- Consistência dos campos CNPJ e FILIAL
- Movimentação das variáveis
- Alterações nos acessos DB2

---

#### 🧾 3. Regras de Formatação de Variáveis

- Campos com `"CPF-CNPJ"`, `"CGC"`, `"CNPJ"` → `PIC 9(09)` → `PIC X(09)`
- Campos com `"FILIAL"` → `PIC 9(05)` → `PIC X(04)`
- Em arquivos FD:
  - Ajustar conforme acima
- NÃO DEVE criar variaveis novas apenas mudar o formato
  
---

#### 🔄 4. Regras de Consistência

- Substituir chamadas à `BRAD0110` por `BRAD2000`
- Validar campos com:
  ```cobol
  IF campo EQUAL SPACES OR campo EQUAL LOW-VALUES
  ```
- Manter tratamento para campos inconsistentes
- Respeitar a identação dos programas
- respeitar as areas A e B do COBOL
- Toda linha adicionada ou comentada deve ser identificada com "4S2511"
- Campos com pictures ZZZ.ZZZ.ZZ9 ou 999.999.999 devem ser trocadas por X(11)

exemplo de implementação BRAD2000

EXEMPLO WORKING-STORAGE SECTION (BRAD2000)
  ```cobol
4S2511 01 WRK-BRAD2000                 PIC  X(08)  VALUE 'BRAD2000'.
ST2511 01 WRK-AREA-BRAD2000.
4S2511    05 WRK-CAD-CPFCGC.
4S2511       10 WRK-CAD-NUMERO         PIC X(09)           VALUE SPACES.
4S2511       10 WRK-CAD-FILIAL         PIC X(04)           VALUE SPACES.
4S2511 77  WRK-CAD-CONTROLE            PIC 9(02)           VALUE ZEROS.

EXEMPLO PROCEDURE DIVISION(BRAD2000)

4S2511*    MOVIMENTAÇÕES PARA OS CAMPOS DA BRAD2000
4S2511     MOVE ZEROS          TO WRK-CAD-CONTROLE. 
4S2511     MOVE WRK-CGC-CPF    TO WRK-CAD-NUMERO.
4S2511     MOVE WRK-FILIAL     TO WRK-CAD-FILIAL.

  ```cobol
4S2511*    CHAMADA DA BRAD2000
4S2511     CALL WRK-BRAD2000     USING WRK-CAD-CPFCGC
4S2511                                 WRK-CAD-CONTROLE.

4S2511* VALIDACAO DO RETORNO DA CHAMADA
           IF RETURN-CODE NOT EQUAL ZEROS
4S2511* CASO OCORRA ALGUM ERRO UTILIZAR O TRATAMENTO DE ERRO PADRAO DO PROGRAMA  
           END-IF.

4S2511* VALIDA SE O CODIGO INFORMADO AO PROGRAMA É IGUAL AO CALCULADO
4S2511     MOVE WRK-CAD-CONTROLE TO WRK-9-02
           IF GFCTLH-CONTROLE NOT EQUAL WRK-9-02-X 
4S2511* CASO OCORRA ALGUM ERRO UTILIZAR O TRATAMENTO DE ERRO PADRAO DO PROGRAMA  
           END-IF.
  ```

---

#### 🧮 5. Movimentação de Variáveis DB2

- Adotar sufixo `-ST` para novos campos DB2
- Exemplos:
  | Original               | Novo                    |
  |------------------------|-------------------------|
  | CCGC-CPF               | CCGC-CPF-ST             |
  | CFLIAL-CGC             | CFLIAL-CGC-ST           |
  | CCTRL-CPF-CGC          | CCTRL-CPF-CGC-ST        |
  | CCNPJ-CPF              | CCNPJ-CPF-ST            |
  | CFLIAL-CNPJ            | CFLIAL-CNPJ-ST          |
  | CCTRL-CNPJ             | CCTRL-CNPJ-ST           |
  | CCNPJ-CPF-DEB          | CCNPJ-CPF-DEB-ST        |
  | CFLIAL-CNPJ-DEB        | CFLIAL-CNPJ-DEB-ST      |
  | CCTRL-CNPJ-CPF-DEB     | CCTRL-CNPJ-CPF-DEB-ST   |
  | CCPF-CNPJ              | CCPF-CNPJ-ST            |
  | CFLIAL-CPF-CNPJ        | CFLIAL-CPF-CNPJ-ST      |
  | CCTRL-CPF-CNPJ         | CCTRL-CPF-CNPJ-ST       |
  | CFLIAL-FORNC           | CFLIAL-FORNC-ST         |
  | CCTRL-FORNC-ST         | CCTRL-FORNC-ST          |

---

#### 🗃️ 6. Alterações em Acessos DB2

- Tabelas alteradas: disponíveis em `.\TABELAS`
- **SELECT/INSERT/UPDATE**: adicionar campos com sufixo `-ST`, sem remover os antigos
- **WHERE**: comentar campos antigos e incluir os novos
- **DELETE**: ajustar cláusula WHERE
- **FETCH**: ajustar cláusula INTO conforme SELECT

---

#### 🗃️ 7. Alterações em Copybooks

- Ajustar apenas os copybooks do DCOM (Identificados como I#DCOMXX ou DCOMWxxx)
- Ajustar formatos conforme regras de formatação
- Comentar campos antigos para referência
- Incluir campos novos com o mesmo nome do antigo, mas com o formato ajustado
- Os copybooks estão no diretório 'C:\Users\herib\OneDrive\Documents\Workspace\Cobol\LIBS\COPYLIB' quando necessário ajuste
copiar o copybook para o diretório 'C:\Users\herib\OneDrive\Documents\Workspace\Cobol\7CO-25-9999-01\DCOM'


---

#### 🚫 8. Restrições

- ❌ Não alterar linhas fora dos padrões indicados
- ❌ Não aplicar refatorações ou melhorias não descritas
- ❌ Não modificar arquivos fora das pastas indicadas
- ❌ Não alterar acesso DB2 em tabelas não listadas

---

#### ✅ 9. Checklist de Alterações

- ✔ Correção de formato de variáveis CNPJ
- ✔ Copybook corrigido
- ✔ Consistência de variáveis ajustada
- ✔ Acessos DB2 corrigidos
